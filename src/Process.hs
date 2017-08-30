{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, ExistentialQuantification, RankNTypes  #-}

module Process (
  safeBracket
) where

import GHC.Generics (Generic)
import Control.Monad as M
import Data.Maybe
import Data.List as L

import Control.Concurrent.MVar
import Control.Concurrent.BoundedChan
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.DeepSeq
import Control.DeepSeq.Generics

import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V
import qualified Data.HashSet as HSet

-- safe-bracket-begin
-- | Fully evaluate the returned result, intercepting and rethrowing all exceptions.
--   If during exception management, the resource `release` produce errors,
--   then only the parent exception of the `compute` function is rethrowned.
--   NOTE: returning a fully evaluated value assures that the resources can be
--   released correctly, because it is not any more needed.
safeBracket
  ::  forall m a b c . (MonadMask m, NFData c)
  => m a
  -- ^ acquire the resource
  -> (Maybe SomeException -> a -> m b)
  -- ^ release the resource at the end of normal result computation, or in case of an exception.
  -> (a -> m c)
  -- ^ compute the result
  -> m c
safeBracket acquire release compute = do
  !resource <- acquire
  withException
    (do !r <- force <$!> compute resource
        release Nothing resource
        return r)
    (\exc -> do release (Just exc) resource
                throw exc)
-- safe-bracket-end

