{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module BuggyLazyEvaluation (
  testBuggyLazyEvaluation
                   ) where

import Process (safeBracket)

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

import Data.Char
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy as LText
import System.IO (openFile, hClose, IOMode(..))

toUpperCase :: LText.Text -> LText.Text
toUpperCase t = LText.map toUpper t

-- bad-bracket-begin
readFileContent1 :: FilePath -> IO LText.Text
readFileContent1 fileName
  = bracket
      (openFile fileName ReadMode)
      hClose
      (\handle -> do content <- LText.hGetContents handle
                     return $ toUpperCase content)
-- bad-bracket-end

-- lazy-begin
printFileContent1 :: FilePath -> IO () 
printFileContent1 fileName = do
  c <- readFileContent1 fileName
  putStrLn $ LText.unpack c
-- lazy-end

-- safe-bracket-begin
readFileContent2 :: FilePath -> IO LText.Text
readFileContent2 fileName
  = safeBracket
      (openFile fileName ReadMode)
      (\_ handle -> hClose handle)
      (\handle -> do content <- LText.hGetContents handle
                     return $ toUpperCase content)
-- safe-bracket-end

printFileContent2 :: FilePath -> IO () 
printFileContent2 fileName = do
  c <- readFileContent2 fileName
  putStrLn $ LText.unpack c

fileName :: FilePath
fileName = "resources/file.txt"

testBuggyLazyEvaluation :: IO ()
testBuggyLazyEvaluation = do
  putStrLn "File content using bad bracket: "
  printFileContent1 fileName
  putStrLn ""
  putStrLn "File content using safe bracket: "
  printFileContent2 fileName
