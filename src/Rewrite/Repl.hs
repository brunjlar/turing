{-# LANGUAGE OverloadedStrings #-}

-- | Terminal user interface helpers for exploring rewrite systems.
--
-- The REPL module renders traces, limits their length for preview purposes,
-- and offers an interactive loop for experimenting with rules.
module Rewrite.Repl
  ( TracePreview (..)
  , renderTraceLines
  , renderTraceLinesLimited
  , renderTracePreview
  , isReloadCommand
  , runRepl
  , runTraceOnce
  , runTraceOnceWithStatus
  ) where

import           Control.Exception      (AsyncException (UserInterrupt), catch,
                                         throwIO)
import           Control.Monad          (when)
import           Data.Foldable          (for_)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Rewrite                (Rules, trace)
import           System.IO              (hFlush, isEOF, stderr, stdout)
import           Text.Read              (readMaybe)

-- | Summary of a trace run suitable for display in the REPL.
data TracePreview = TracePreview
  { previewLines     :: [T.Text]  -- ^ Rendered rewrite steps.
  , previewTruncated :: Bool      -- ^ Whether more steps were suppressed.
  } deriving (Eq, Show)

-- | Render the full trace for an input string with step numbers.
renderTraceLines :: Rules Char -> String -> [T.Text]
renderTraceLines rules input = zipWith format [0 :: Int ..] (trace rules input)
  where
    format idx step = T.pack ("step " <> show idx <> ": " <> step)

-- | Render trace lines but disregard any truncation state.
renderTraceLinesLimited :: Maybe Int -> Rules Char -> String -> [T.Text]
renderTraceLinesLimited maybeLimit rules input =
  previewLines (renderTracePreview maybeLimit rules input)

-- | Compute a truncated preview of a trace respecting the optional limit.
renderTracePreview :: Maybe Int -> Rules Char -> String -> TracePreview
renderTracePreview maybeLimit rules input =
  case normalizedLimit of
    Nothing    -> TracePreview traced False
    Just limit ->
      let (shown, remainder) = splitAt (succ limit) traced
       in TracePreview shown (not (null remainder))
  where
    traced = renderTraceLines rules input
    normalizedLimit = normalizeLimit maybeLimit

-- | Run a single trace, discarding the truncation status.
runTraceOnce :: Rules Char -> Maybe Int -> String -> IO ()
runTraceOnce rules maybeLimit input = do
  _ <- runTraceOnceWithStatus rules maybeLimit input
  pure ()

-- | Run a single trace and return the resulting preview.
runTraceOnceWithStatus :: Rules Char -> Maybe Int -> String -> IO TracePreview
runTraceOnceWithStatus rules maybeLimit input = do
  let preview = renderTracePreview maybeLimit rules input
  for_ (previewLines preview) TIO.putStrLn
  pure preview

-- | Control character used to trigger rule reloads.
reloadKey :: Char
reloadKey = '\x12'

-- | Control character used to change the step limit.
setLimitKey :: Char
setLimitKey = '\x13'

-- | Detect commands that request a rules reload.
isReloadCommand :: String -> Bool
isReloadCommand input = input == [reloadKey] || input == "^R" || input == ":reload"

-- | Run the interactive REPL, reloading rules and adjusting limits on demand.
runRepl :: IO (Either T.Text (Rules Char)) -> Maybe Int -> Rules Char -> IO ()
runRepl reloadRules initialLimit initialRules = do
  putStrLn "Enter strings to trace. Press Ctrl-C to exit."
  putStrLn "Use Ctrl-R to reload rules. Press Ctrl-S to set a max step limit (0 = unlimited)."
  reportLimit normalizedInitial
  rulesRef <- newIORef initialRules
  limitRef <- newIORef normalizedInitial
  loop rulesRef limitRef `catch` exitOnInterrupt
  where
    normalizedInitial = normalizeLimit initialLimit

    loop :: IORef (Rules Char) -> IORef (Maybe Int) -> IO ()
    loop rulesRef limitRef = do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      if eof
        then putStrLn "" -- exit on EOF (Ctrl-D)
        else do
          line <- getLine
          if isReloadCommand line
            then do
              result <- reloadRules
              case result of
                Left err -> TIO.hPutStrLn stderr err
                Right rules -> do
                  writeIORef rulesRef rules
                  printTracePreview rules
              loop rulesRef limitRef
            else if isSetLimitCommand line
              then do
                maybeNewLimit <- promptForLimit
                case maybeNewLimit of
                  Nothing -> pure ()
                  Just newLimit -> do
                    writeIORef limitRef newLimit
                    reportLimit newLimit
                loop rulesRef limitRef
              else do
                rules <- readIORef rulesRef
                limit <- readIORef limitRef
                continue <- catch (do
                    preview <- runTraceOnceWithStatus rules limit line
                    when (previewTruncated preview) (reportTruncated limit)
                    pure True
                  ) traceInterrupted
                when continue (loop rulesRef limitRef)

    printTracePreview :: Rules Char -> IO ()
    printTracePreview = print

    traceInterrupted :: AsyncException -> IO Bool
    traceInterrupted UserInterrupt = do
      putStrLn "\nTrace interrupted."
      pure True
    traceInterrupted e = throwIO e

    exitOnInterrupt :: AsyncException -> IO ()
    exitOnInterrupt UserInterrupt = putStrLn "\nInterrupted."
    exitOnInterrupt e             = throwIO e

    reportLimit :: Maybe Int -> IO ()
    reportLimit maybeLimit =
      putStrLn $ "Current step limit: " <> maybe "unlimited" show maybeLimit

    isSetLimitCommand :: String -> Bool
    isSetLimitCommand input = input == [setLimitKey] || input == "^S" || input == ":steps"

    promptForLimit :: IO (Maybe (Maybe Int))
    promptForLimit = do
      putStr "Set max steps (0 = unlimited): "
      hFlush stdout
      eof <- isEOF
      if eof
        then do
          putStrLn ""
          pure Nothing
        else do
          response <- getLine
          case parseLimit response of
            Nothing -> do
              putStrLn "Please enter a natural number."
              pure Nothing
            Just parsed -> pure (Just parsed)

    reportTruncated :: Maybe Int -> IO ()
    reportTruncated maybeLimit = do
      let limitText = maybe "unknown" show maybeLimit
      putStrLn $ "Trace truncated after " <> limitText <> " step(s). Increase the limit with Ctrl-S to see more."

-- | Normalise the optional limit, treating zero and negatives as unlimited.
normalizeLimit :: Maybe Int -> Maybe Int
normalizeLimit maybeLimit = maybeLimit >>= toLimit
  where
    toLimit n
      | n <= 0    = Nothing
      | otherwise = Just n

-- | Parse user input into an optional limit, rejecting negatives.
parseLimit :: String -> Maybe (Maybe Int)
parseLimit input = do
  number <- readMaybe input :: Maybe Int
  if number < 0
    then Nothing
    else Just (normalizeLimit (Just number))
