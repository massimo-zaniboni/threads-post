{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-} 

module Channels (
    OneChanType(..)
  , testChannels
  , process_manager
  ) where

import Common

import System.Random 
import System.Mem
import Data.Hashable
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.BoundedChan as BChan
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.DeepSeq 
import qualified Data.Vector.Unboxed as BV 
import qualified Process as Safe
import Data.List as L
import Data.IORef
import System.Random
import Control.Concurrent (threadDelay)
import Control.DeepSeq
import GHC.Generics (Generic)

-- -------------------------------------
-- Support different types of channels

-- chan-types-begin
data OneChan a
  = ChanDefault (Chan.Chan (Maybe a))
  | ChanBounded (BChan.BoundedChan (Maybe a))
  | ChanMVar (MVar (Maybe a))
-- chan-types-end

readFromOneChan :: OneChan a -> IO (Maybe a)
readFromOneChan chan
  = case chan of
      ChanDefault ch -> Chan.readChan ch
      ChanBounded ch -> BChan.readChan ch
      ChanMVar ch -> takeMVar ch

writeToOneChan :: OneChan a -> Maybe a -> IO ()
writeToOneChan chan x
  = case chan of
      ChanDefault ch -> Chan.writeChan ch x
      ChanBounded ch -> BChan.writeChan ch x
      ChanMVar ch -> putMVar ch x

-- -----------------------------
-- Test memory usage

-- producer-begin
-- | Generate vectors of values.
fastProducer :: Int -> Int -> Int -> OneChan RandomBV -> IO ()
fastProducer seed vectorDimension channelDimension chan = do
  case channelDimension of
    0 -> do writeToOneChan chan Nothing
            return ()
    _ -> do let bv = randomBV seed vectorDimension
            writeToOneChan chan (Just bv)
            putStrLn $ "producer: " ++ show channelDimension ++ " left elements to write" ++ ", hash: " ++ show (hash bv)
            fastProducer (seed + 1) vectorDimension (channelDimension - 1) chan
-- producer-end


-- consumer-begin
-- | Slowly consume values.
slowConsumer :: Int -> OneChan RandomBV -> IO ()
slowConsumer fuzzyNormalizer chan = do
  mbv <- readFromOneChan chan
  case mbv of
    Nothing
      -> do return ()
    Just bv
      -> do let r = slowComputationOnVector bv fuzzyNormalizer
            putStrLn $ "consumer: " ++ show r
            performMinorGC
            performMajorGC
            slowConsumer fuzzyNormalizer chan
-- consumer-end

-- | Decide the type of channel to use
data OneChanType
  = OneDefaultChan
  | OneBoundedChan
  | OneMVarChan
 deriving (Eq, Show)

-- test-begin
-- | Test the channels.
--   NOTE: a seed is passed as argument, so the compiler can not optimize the code in advance.
testChannels :: OneChanType -> Int -> Int -> Int -> Int -> IO ()
testChannels chanType seed fuzzyNormalizer vectorDimension channelDimension = do
  chan :: OneChan RandomBV
    <- case chanType of
          OneDefaultChan
            -> ChanDefault <$> Chan.newChan
          OneBoundedChan
            -> ChanBounded <$> BChan.newBoundedChan 2
          OneMVarChan
            -> ChanMVar <$> newEmptyMVar

  _ <- concurrently
         (fastProducer seed vectorDimension channelDimension chan)
         (slowConsumer fuzzyNormalizer chan)
  return ()
-- test-end

-- --------------------------------
-- Test deadlock bug

-- TODO switch to multi channels type 

data Cmd = Cmd Int
 deriving(Eq, Generic, NFData)

process_readFromDB :: Int -> Int -> Safe.Chan Cmd -> IO ()
process_readFromDB seed nrOfElements ch = do
  mapM_ (\s -> Safe.writeChan ch (Cmd s)) (L.take nrOfElements $ randoms $ mkStdGen seed)
  Safe.writeEOFChan ch

process_transformData :: Int -> Int -> Safe.Chan Cmd -> Safe.Chan Cmd -> IO ()
process_transformData fuzzyNormalizer waitTime inCh outCh  = do
  maybeCmd <- Safe.readChan inCh
  case maybeCmd of
    Nothing
      -> do Safe.writeEOFChan outCh
    Just (Cmd s)
      -> do case waitTime > 0 of
              True -> threadDelay waitTime
              False -> return ()
            Safe.writeChan outCh (Cmd $ s `mod` fuzzyNormalizer)
            process_transformData fuzzyNormalizer waitTime inCh outCh

process_writeToDB :: Int -> Int -> Safe.Chan Cmd -> IO ()
process_writeToDB fuzzyNormalizer waitTime ch = do
  initialState <- newIORef 0
  processCmds initialState

 where

   processCmds stateR = do
     s0 <- readIORef stateR
     maybeCmd <- Safe.readChan ch
     case maybeCmd of
       Nothing -> do putStrLn $ "process_writeToDB last state: " ++ show s0
                     return ()
       Just (Cmd s)
           -> do case waitTime > 0 of
                   True -> threadDelay waitTime
                   False -> return ()
                 writeIORef stateR ((s0 + s) `mod` fuzzyNormalizer)
                 processCmds stateR

process_manager :: Int -> Int -> Int -> Int -> Int -> IO ()
process_manager seed fuzzyNormalizer producerWaitTime consumerWaitTime nrOfElements = do
  chan1 <- Safe.newChan 
  chan2 <- Safe.newChan 

  _ <- withAsync
         (process_writeToDB fuzzyNormalizer consumerWaitTime chan2)
         (\writeThread -> do
             _ <- concurrently
                    (process_readFromDB seed nrOfElements chan1)
                    (process_transformData fuzzyNormalizer producerWaitTime chan1 chan2)

             wait writeThread)
  return ()
