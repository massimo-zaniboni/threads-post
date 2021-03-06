{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-} 

module Optimizations(
   processVectorsBV
 , fusionOnListFoldl
 , fusionOnListFoldl'
 , fusionOnListFoldr
 , fusedOnListFoldl
 , fusedOnListFoldl'
 , fusionOnStream
 , fusedOnListFoldr
 , fusedOnStream
 , sumOnStream
 , sumOnChan
 , sumOnVector
 , sumOnUnagi
 , sumUsingTasks
             ) where

import Common
import Process

import Data.List as L
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.Vector.Unboxed as BV 
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Statistics.Types
import Data.Hashable
import System.Random
import qualified Data.Vector.Unboxed as BV 
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.Vector as S
import qualified Control.Concurrent.Chan.Unagi as UStream

-- --------------------------------
-- Bench lazy vs strict

-- process-begin
processVectorsBV :: Int -> Int -> Int -> Bool -> Int -> Int
processVectorsBV !seed !vectorDimension !numberOfVectors !useStrictAlgo !lastSum
  = case numberOfVectors of
      0 -> lastSum
      _ -> let bv = randomBV seed vectorDimension
               !h = case useStrictAlgo of
                     True -> slowComputationOnVector bv seed
                     False -> slowComputationOnVector2 bv seed
               !r = processVectorsBV (seed + 1) vectorDimension (numberOfVectors - 1) useStrictAlgo (lastSum + h)
           in r
-- process-end

-- -------------------------------
-- Bench stream fusion

-- fusion-list-begin
fusionOnListFoldl :: [Int] -> Int
fusionOnListFoldl l = L.foldl (+) 0 $ map (* 2) $ filter odd l

fusionOnListFoldl' :: [Int] -> Int
fusionOnListFoldl' l = L.foldl' (+) 0 $ map (* 2) $ filter odd l

fusionOnListFoldr :: [Int] -> Int
fusionOnListFoldr l = L.foldr (+) 0 $ map (* 2) $ filter odd l

fusedCalcOnFoldl :: Int -> Int -> Int
fusedCalcOnFoldl s x
  = case odd x of
      True -> s + (x * 2)
      False -> s
{-# INLINE fusedCalcOnFoldl #-}

fusedCalcOnFoldr :: Int -> Int -> Int
fusedCalcOnFoldr x s = fusedCalcOnFoldl s x
{-# INLINE fusedCalcOnFoldr #-}

fusedOnListFoldl' :: [Int] -> Int
fusedOnListFoldl' l = L.foldl' fusedCalcOnFoldl 0 l

fusedOnListFoldl :: [Int] -> Int
fusedOnListFoldl l = L.foldl fusedCalcOnFoldl 0 l

fusedOnListFoldr :: [Int] -> Int
fusedOnListFoldr l = L.foldr fusedCalcOnFoldr 0 l
-- fusion-list-end

-- fusion-stream-begin
fusionOnStream :: [Int] -> IO Int
fusionOnStream l = do
  inS1 <- S.fromList l
  inS2 <- S.filter odd inS1
  inS3 <- S.map (* 2) inS2
  S.fold (+) 0 inS3

fusedOnStream :: [Int] -> IO Int
fusedOnStream l = do
  inS1 <- S.fromList l
  S.fold fusedCalcOnFoldl 0 inS1
-- fusion-stream-end

-- -----------------------------------
-- Bench channels vs stream vs vector

-- sumOnStream-begin
sumOnStream :: BV.Vector Int -> IO Int
sumOnStream l = do
  inS1 <- S.fromVector l
  S.fold fusedCalcOnFoldl 0 inS1
-- sumOnStream-end

-- sumOnVector-begin
sumOnVector :: BV.Vector Int -> Int
sumOnVector l = BV.foldl' fusedCalcOnFoldl 0 l
-- sumOnVector-end

-- sumOnChan-begin
sumOnChan :: BV.Vector Int -> IO Int
sumOnChan l = do
  ch <- newChan

  (_, r) <- concurrently
              (do BV.mapM_ (writeChan ch) l
                  writeEOFChan ch)
              (sumOnChan' ch 0)
  return r
 where
   
   sumOnChan' ch !s = do
     mx <- readChan ch
     case mx of
       Nothing -> return s
       Just x -> sumOnChan' ch (fusedCalcOnFoldl s x)
-- sumOnChan-end

-- sumOnUnagi-begin
sumOnUnagi :: BV.Vector Int -> IO Int
sumOnUnagi l = do
  (inCh, outCh) <- UStream.newChan

  (_, r) <- concurrently
              (do BV.mapM_ (\i -> UStream.writeChan inCh (Just i)) l
                  UStream.writeChan inCh Nothing)
              (sumOnChan' outCh 0)
  return r
 where
   
   sumOnChan' ch !s = do
     mx <- UStream.readChan ch
     case mx of
       Nothing -> return s
       Just x -> sumOnChan' ch (fusedCalcOnFoldl s x)
-- sumOnUnagi-end

-- ---------------------------
-- Threads as Task

-- sumUsingTasks-begin
sumUsingTasks :: BV.Vector Int -> IO Int
sumUsingTasks l = do

  finalResult :: MVar Int <- newEmptyMVar

  currentState :: MVar (Int, Int) <- newEmptyMVar

  nrOfElements <- BV.foldM (\i x -> do forkIO $ sumTask x finalResult currentState
                                       return $ i + 1) 0 l
  
  putMVar currentState (0, nrOfElements)

  takeMVar finalResult
  
 where

   sumTask :: Int -> MVar Int -> MVar (Int, Int) -> IO ()
   sumTask x finalResult currentState = do
     (s1, leftSums) <- takeMVar currentState
     let !currentState2@(!s2, !leftSums2) = (fusedCalcOnFoldl s1 x, leftSums - 1)
     case leftSums2 of
       0 -> putMVar finalResult s2
       _ -> putMVar currentState currentState2 
   
-- sumUsingTasks-end

