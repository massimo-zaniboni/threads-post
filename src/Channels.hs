{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Channels (
    OneChanType(..)
  , testChannels
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
-- Process

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
