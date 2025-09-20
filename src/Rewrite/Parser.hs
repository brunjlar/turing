{-# LANGUAGE OverloadedStrings #-}

module Rewrite.Parser
  ( parseRules
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Bifunctor             (first)
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, chunk, eof,
                                             errorBundlePretty, lookAhead, many,
                                             optional, runParser, satisfy,
                                             takeWhileP, try, (<?>))
import           Text.Megaparsec.Char       (char)
import qualified Text.Megaparsec.Char.Lexer as L

import           Rewrite                    (Rule (..), Rules)

type Parser = Parsec Void Text

data Piece = Piece
  { pieceChar    :: !Char
  , pieceEscaped :: !Bool
  }

data Context = Context [Char] [Char] !Bool

parseRules :: Text -> Either Text (Rules Char)
parseRules input =
  first (T.pack . errorBundlePretty) $ runParser rulesP "rules" input

rulesP :: Parser (Rules Char)
rulesP = do
  sc
  rs <- many (ruleP <* sc)
  void (takeWhileP Nothing isInlineSpace)
  eof
  pure rs

ruleP :: Parser (Rule Char)
ruleP = do
  lhs <- ruleText "->"
  void (chunk "->")
  rhs <- ruleText ";"
  void (char ';')
  pure (Rule (T.unpack lhs) (T.unpack rhs))

ruleText :: Text -> Parser Text
ruleText terminator = do
  ctx <- ruleTextPieces terminator (Context [] [] False)
  pure $ finalize ctx

ruleTextPieces :: Text -> Context -> Parser Context
ruleTextPieces terminator ctx0 = do
  ctx1 <- consumeSpacesAndComments ctx0
  endHere <- optional (try (lookAhead (chunk terminator)))
  case endHere of
    Just _  -> pure ctx1
    Nothing -> do
      piece <- pieceParser
      let ctx2 = commitPiece ctx1 piece
      ruleTextPieces terminator ctx2

pieceParser :: Parser Piece
pieceParser = escapedPiece <|> plainPiece

escapedPiece :: Parser Piece
escapedPiece = do
  void (char '\\')
  c <- anyChar "escape sequence"
  pure (Piece (interpretEscape c) True)

plainPiece :: Parser Piece
plainPiece = do
  c <- anyChar "character"
  pure (Piece c False)

anyChar :: String -> Parser Char
anyChar label = satisfy (const True) <?> label

interpretEscape :: Char -> Char
interpretEscape 'n'   = '\n'
interpretEscape 't'   = '\t'
interpretEscape 'r'   = '\r'
interpretEscape 's'   = ' '
interpretEscape other = other

commitPiece :: Context -> Piece -> Context
commitPiece (Context resRev pendingRev hasContent) piece
  | isWhitespace piece && not (pieceEscaped piece) =
      Context resRev (pieceChar piece : pendingRev) hasContent
  | hasContent =
      let resWithPending = flushPending resRev pendingRev
          newResRev       = pieceChar piece : resWithPending
      in Context newResRev [] True
  | otherwise =
      Context (pieceChar piece : resRev) [] True

flushPending :: [Char] -> [Char] -> [Char]
flushPending resRev pendingRev =
  List.foldl' (flip (:)) resRev (reverse pendingRev)

finalize :: Context -> Text
finalize (Context resRev pendingRev hasContent)
  | hasContent = T.pack . reverse $ resRev
  | otherwise  = T.pack . reverse $ flushPending resRev pendingRev

isWhitespace :: Piece -> Bool
isWhitespace = isInlineSpace . pieceChar

isInlineSpace :: Char -> Bool
isInlineSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

consumeSpacesAndComments :: Context -> Parser Context
consumeSpacesAndComments ctx = do
  ctxAfterSpaces <- consumeSpaces ctx
  commentPresent <- optional (try comment)
  case commentPresent of
    Just _  -> consumeSpacesAndComments ctxAfterSpaces
    Nothing -> pure ctxAfterSpaces

consumeSpaces :: Context -> Parser Context
consumeSpaces (Context resRev pendingRev hasContent) = do
  spacesTxt <- takeWhileP (Just "space") isInlineSpace
  let pending' = T.foldl' (flip (:)) pendingRev spacesTxt
  pure (Context resRev pending' hasContent)

comment :: Parser ()
comment = L.skipLineComment "--" <|> L.skipBlockCommentNested "{-" "-}"

sc :: Parser ()
sc = void $ many (blankLine <|> commentChunk)
  where
    blankLine = try $ do
      void (takeWhileP Nothing isHorizontalSpace)
      void (char '\n')
    commentChunk = try $ do
      void (takeWhileP Nothing isHorizontalSpace)
      comment
      void (takeWhileP Nothing isHorizontalSpace)
      void (optional (char '\n'))

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t' || c == '\r'
