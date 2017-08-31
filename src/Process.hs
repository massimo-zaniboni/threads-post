{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, ExistentialQuantification, RankNTypes  #-}

module Process (
    safeBracket
  , writeChan
  , readChan
  , writeEOFChan
  , newChan
  , Chan
) where

import GHC.Generics (Generic)
import Control.Monad as M
import Data.Maybe
import Data.List as L

import Control.Concurrent.MVar
import qualified Control.Concurrent.BoundedChan as BChan
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.DeepSeq
import Control.DeepSeq.Generics

-- --------------------------------
-- Safe resource management

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

-- ----------------------------------
-- Concurrent process

-- safe-channels-begin
-- | A channel of fully evaluated (NFData) values.
newtype Chan a = Chan (BChan.BoundedChan (Maybe a))

-- | A channel bounded to number of cores + 1.
--   The channel is bounded so a producer faster than consumers
--   does not waste the memory filling the channel with new values to process.
newChan :: IO (Chan a)
newChan = do
  dim <- getNumCapabilities
  Chan <$> BChan.newBoundedChan (dim + 1)

{-# INLINE writeChan #-}
-- | Fully evaluate the value, and write on the channel.
writeChan :: (NFData a) => Chan a -> a -> IO ()
writeChan (Chan ch) v = BChan.writeChan ch (Just $ force v)

{-# INLINE writeEOFChan #-}
-- | When the producer send this command, it informs consumers that there are no any more
--   interesting values.
writeEOFChan :: Chan a -> IO ()
writeEOFChan (Chan ch) = BChan.writeChan ch Nothing

{-# INLINE readChan #-}
-- | Read the next value.
--   Nothing implies that no new values should be put on the channel.
readChan :: Chan a -> IO (Maybe a)
readChan (Chan ch) = BChan.readChan ch
-- safe-channels-end
