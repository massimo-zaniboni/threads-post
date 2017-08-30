{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module BadChannels (
  testOverflowChannel
                   ) where

import Common

import System.Mem
import Data.Hashable
import System.Random
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Concurrent.Async
import qualified Data.Vector.Unboxed as BV 
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

