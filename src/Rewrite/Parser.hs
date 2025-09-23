{-# LANGUAGE OverloadedStrings #-}

-- | Parser for textual rewrite rule descriptions.
--
-- The parser accepts the compact syntax used throughout the tutorial and
-- returns structured rewrite rules that can be fed to the tracing engine.
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

-- | Megaparsec parser specialised for textual rule definitions.
type Parser = Parsec Void Text

-- | A single parsed character, tracking whether it originated from an escape
-- sequence.
data Piece = Piece
  { pieceChar    :: !Char
  , pieceEscaped :: !Bool
  }

-- | Parser state tracking the characters collected so far, pending whitespace
-- and whether any non-space characters have been observed.
data Context = Context [Char] [Char] !Bool

-- | Parse an entire rules file into a list of rewrite rules, returning a
-- pretty-printed error message on failure.
parseRules :: Text -> Either Text (Rules Char)
parseRules input =
  first (T.pack . errorBundlePretty) $ runParser rulesP "rules" input

rulesP :: Parser (Rules Char)
-- | Parse zero or more rules separated by whitespace and comments.
rulesP = do
  sc
  rs <- many (ruleP <* sc)
  void (takeWhileP Nothing isInlineSpace)
  eof
  pure rs

ruleP :: Parser (Rule Char)
-- | Parse a single @lhs -> rhs;@ rewrite clause.
ruleP = do
  lhs <- ruleText "->"
  void (chunk "->")
  rhs <- ruleText ";"
  void (char ';')
  pure (Rule (T.unpack lhs) (T.unpack rhs))

ruleText :: Text -> Parser Text
-- | Parse the text of a left- or right-hand side, up to the given terminator.
ruleText terminator = do
  ctx <- ruleTextPieces terminator (Context [] [] False)
  pure $ finalize ctx

ruleTextPieces :: Text -> Context -> Parser Context
-- | Accumulate characters belonging to a rule side, respecting escapes and
-- comments until the terminator is encountered.
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
-- | Parse either a plain character or an escaped character.
pieceParser = escapedPiece <|> plainPiece

escapedPiece :: Parser Piece
-- | Parse a backslash escape sequence.
escapedPiece = do
  void (char '\\')
  c <- anyChar "escape sequence"
  pure (Piece (interpretEscape c) True)

plainPiece :: Parser Piece
-- | Parse a non-escaped character.
plainPiece = do
  c <- anyChar "character"
  pure (Piece c False)

anyChar :: String -> Parser Char
-- | Accept any single character, using the provided label for error messages.
anyChar label = satisfy (const True) <?> label

interpretEscape :: Char -> Char
-- | Interpret supported escape sequences used in rewrite files.
interpretEscape 'n'   = '\n'
interpretEscape 't'   = '\t'
interpretEscape 'r'   = '\r'
interpretEscape 's'   = ' '
interpretEscape other = other

commitPiece :: Context -> Piece -> Context
-- | Incorporate a parsed piece into the running context, tracking whitespace
-- boundaries and escaped characters.
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
-- | Move pending whitespace into the main accumulator.
flushPending resRev pendingRev =
  List.foldl' (flip (:)) resRev (reverse pendingRev)

finalize :: Context -> Text
-- | Convert the accumulated context into the final text for a rule side.
finalize (Context resRev pendingRev hasContent)
  | hasContent = T.pack . reverse $ resRev
  | otherwise  = T.pack . reverse $ flushPending resRev pendingRev

isWhitespace :: Piece -> Bool
-- | Test whether a parsed piece represents whitespace.
isWhitespace = isInlineSpace . pieceChar

isInlineSpace :: Char -> Bool
-- | Identify inline whitespace characters that may be elided.
isInlineSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

consumeSpacesAndComments :: Context -> Parser Context
-- | Consume interleaved whitespace and comments ahead of a rule token.
consumeSpacesAndComments ctx = do
  ctxAfterSpaces <- consumeSpaces ctx
  commentPresent <- optional (try comment)
  case commentPresent of
    Just _  -> consumeSpacesAndComments ctxAfterSpaces
    Nothing -> pure ctxAfterSpaces

consumeSpaces :: Context -> Parser Context
-- | Consume consecutive space characters, appending them to the pending buffer.
consumeSpaces (Context resRev pendingRev hasContent) = do
  spacesTxt <- takeWhileP (Just "space") isInlineSpace
  let pending' = T.foldl' (flip (:)) pendingRev spacesTxt
  pure (Context resRev pending' hasContent)

comment :: Parser ()
-- | Parse either a line or nested block comment.
comment = L.skipLineComment "--" <|> L.skipBlockCommentNested "{-" "-}"

sc :: Parser ()
-- | Skip whitespace and comments surrounding top-level rules.
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
-- | Recognise horizontal whitespace characters.
isHorizontalSpace c = c == ' ' || c == '\t' || c == '\r'
