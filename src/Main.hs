{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Main (
    main
) where

import Common
import Channels 
import BuggyLazyEvaluation
import qualified Optimizations as Optimizations

import System.Environment
import System.Exit (die)
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types

-- NOTE: many params are passed from the command-line, so the compiler
-- can not optimize (cheating) the code in advance.

main = do
  args <- getArgs
  case args of
    ["test", "channels", chanTypeS, seedS, vectorDimensionS, channelDimensionS]
      -> do let seed :: Int = read seedS
            let vectorDimension :: Int = read vectorDimensionS
            let channelDimension :: Int = read channelDimensionS
            let fuzzyNormalizer = 12456789
            let chanType = case chanTypeS of
                             "default"
                               -> OneDefaultChan
                             "bounded"
                               -> OneBoundedChan
                             "mvar"
                               -> OneMVarChan
            testChannels chanType seed fuzzyNormalizer vectorDimension channelDimension

    ["bench", "strict1", seedS, vectorDimensionS, channelDimensionS, useStrictS]
      -> do let seed :: Int = read seedS
            let vectorDimension :: Int = read vectorDimensionS
            let channelDimension :: Int = read channelDimensionS
            let useStrict = case useStrictS of
                              "0" -> False
                              "1" -> True

            let f = Optimizations.processVectorsBV seed vectorDimension channelDimension useStrict

            runMode
             (Run defaultConfig Prefix [""])
             [bench "processVectorsBV" $ nf f 0]

            putStrLn $ "processVectorsBV result: " ++ show (f 0)

    ["test", "buggy-lazy-evaluation"]
      -> testBuggyLazyEvaluation

    _ -> die "Unrecognized options."
