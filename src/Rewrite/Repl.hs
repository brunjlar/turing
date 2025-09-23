{-# LANGUAGE OverloadedStrings #-}

module Rewrite.Repl
  ( renderTraceLines
  , renderTraceLinesLimited
  , isReloadCommand
  , runRepl
  , runTraceOnce
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

renderTraceLines :: Rules Char -> String -> [T.Text]
renderTraceLines rules input = zipWith format [0 :: Int ..] (trace rules input)
  where
    format idx step = T.pack ("step " <> show idx <> ": " <> step)

renderTraceLinesLimited :: Maybe Int -> Rules Char -> String -> [T.Text]
renderTraceLinesLimited maybeLimit rules input = applyLimit (renderTraceLines rules input)
  where
    applyLimit = maybe id (take . succ) maybeLimit

runTraceOnce :: Rules Char -> Maybe Int -> String -> IO ()
runTraceOnce rules maybeLimit input =
  for_ (renderTraceLinesLimited maybeLimit rules input) TIO.putStrLn

reloadKey :: Char
reloadKey = '\x12'

isReloadCommand :: String -> Bool
isReloadCommand input = input == [reloadKey] || input == "^R" || input == ":reload"

runRepl :: IO (Either T.Text (Rules Char)) -> Rules Char -> IO ()
runRepl reloadRules initialRules = do
  putStrLn "Enter strings to trace. Press Ctrl-C to exit."
  rulesRef <- newIORef initialRules
  loop rulesRef `catch` exitOnInterrupt
  where
    loop :: IORef (Rules Char) -> IO ()
    loop rulesRef = do
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
              loop rulesRef
            else do
              rules <- readIORef rulesRef
              continue <- catch (printTrace rules line >> pure True) traceInterrupted
              when continue (loop rulesRef)

    printTrace :: Rules Char -> String -> IO ()
    printTrace rules line = runTraceOnce rules Nothing line

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
