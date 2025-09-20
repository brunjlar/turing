{-# LANGUAGE OverloadedStrings #-}

module Rewrite.Repl
  ( renderTraceLines
  , renderTraceLinesLimited
  , runRepl
  , runTraceOnce
  ) where

import           Control.Exception      (AsyncException (UserInterrupt), catch,
                                         throwIO)
import           Control.Monad          (when)
import           Data.Foldable          (for_)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Rewrite                (Rules, trace)
import           System.IO              (hFlush, isEOF, stdout)

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

runRepl :: Rules Char -> IO ()
runRepl rules = do
  putStrLn "Enter strings to trace. Press Ctrl-C to exit."
  loop `catch` exitOnInterrupt
  where
    loop :: IO ()
    loop = do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      if eof
        then putStrLn "" -- exit on EOF (Ctrl-D)
        else do
          line <- getLine
          continue <- catch (printTrace line >> pure True) traceInterrupted
          when continue loop

    printTrace :: String -> IO ()
    printTrace line = runTraceOnce rules Nothing line

    traceInterrupted :: AsyncException -> IO Bool
    traceInterrupted UserInterrupt = do
      putStrLn "\nTrace interrupted."
      pure True
    traceInterrupted e = throwIO e

    exitOnInterrupt :: AsyncException -> IO ()
    exitOnInterrupt UserInterrupt = putStrLn "\nInterrupted."
    exitOnInterrupt e             = throwIO e
