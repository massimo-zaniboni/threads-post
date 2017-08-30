{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-} 

module Optimizations(
  processVectorsBV
             ) where

import Common

import Control.Concurrent.Async
import qualified Data.Vector.Unboxed as BV 
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

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

