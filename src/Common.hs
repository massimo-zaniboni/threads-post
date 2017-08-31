{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-} 

module Common(
    RandomBV
  , randomBV
  , slowComputationOnVector
  , slowComputationOnVector2
  , FullyRandomBV(..)
  , fullyRandomBV
  , slow
  , fast
             ) where

import Data.Hashable
import System.Random
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.Async
import qualified Data.Vector.Unboxed as BV 
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-- --------------------------------
-- RandomBV


type RandomBV = BV.Vector Int

instance Hashable RandomBV where
  hashWithSalt s0 v = BV.foldl' (\s1 s2 -> s1 + (hash s2)) s0 v 

randomBV
    :: Int
    -- ^ the seed to use
    -> Int
    -- ^ the dimension of the vector 
    -> RandomBV
    -- ^ a vector containing semi-random numbers

randomBV seed d =
  BV.fromListN d $ randoms $ mkStdGen seed


-- | Perform a computation on RandomBV that is for sure slower than its generation,
--   because it repeats many times the random number generation.
slowComputationOnVector
    :: RandomBV
    -> Int
    -- ^ add some noise to the calcs, so the compiler can not optimize the code too much
    -- @require this > 1000
    -> Int
slowComputationOnVector bv fuzzyNormalizer
  = let g0 = mkStdGen (fuzzyNormalizer * 5)

-- strict-begin
        nextR :: Int -> (Int, StdGen) -> (Int, StdGen)
        nextR 0 (!x1, !g1) = (x1, g1)
        nextR n (!x1, !g1) = nextR (n - 1) (random g1)

        maxRandRepetition :: Int
        maxRandRepetition = 20

        advance :: (Int, StdGen) -> Int -> (Int, StdGen)
        advance (!s1, !g1) !x1
          = let (!x2, !g2) = nextR (x1 `mod` maxRandRepetition) (x1, g1)
            in  ((s1 + x2) `mod` fuzzyNormalizer, g2)
-- strict-end
    in  fst $ BV.foldl' advance (0, g0) bv

-- | The same as `slowComputationOnVector` but without strict annotations.
-- comp2-begin
slowComputationOnVector2
    :: RandomBV
    -> Int
    -- ^ add some noise to the calcs, so the compiler can not optimize the code too much
    -- @require this > 1000
    -> Int
slowComputationOnVector2 bv fuzzyNormalizer
  = let g0 = mkStdGen (fuzzyNormalizer * 5)

        nextR :: Int -> (Int, StdGen) -> (Int, StdGen)
        nextR 0 (x1, g1) = (x1, g1)
        nextR n (x1, g1) = nextR (n - 1) (random g1)

        maxRandRepetition :: Int
        maxRandRepetition = 20

        advance :: (Int, StdGen) -> Int -> (Int, StdGen)
        advance (s1, g1) x1
          = let (x2, g2) = nextR (x1 `mod` maxRandRepetition) (x1, g1)
            in  ((s1 + x2) `mod` fuzzyNormalizer, g2)

    in  fst $ BV.foldl' advance (0, g0) bv
-- comp2-end

-- --------------------------
-- FullyRandomBV

-- MAYBE delete

-- | A fully random Boxed Vector of Integers.
--   It is a lot slower to generate, respect RandomBV.
newtype FullyRandomBV = FullyRandomBV (BV.Vector Int)
 deriving (Eq, Generic, NFData)

-- | A more CPU intensive version of Hash to calculate.
instance Hashable FullyRandomBV where
  hashWithSalt s0 (FullyRandomBV v)
      = BV.foldl' (\s1 s2
                       -> case mod s2 4 of
                            0 -> s1 + s2
                            1 -> s1 * s2
                            2 -> div s1 s2
                            3 -> s1 - s2) s0 v

fullyRandomBV
    :: Int
    -- ^ the dimension of the vector 
    -> IO FullyRandomBV
    -- ^ a vector containing random numbers

fullyRandomBV d = FullyRandomBV <$> BV.generateM d (\_ -> randomIO)

-- ----------------------------
-- Params

slow :: Int
slow = 10 * 10^6

fast :: Int
fast = 10^3

