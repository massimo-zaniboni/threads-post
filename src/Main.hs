{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Main (
    main
) where

import Common
import Channels 
import BuggyLazyEvaluation

import qualified Optimizations as Optimizations
import Control.DeepSeq
import System.Random
import Data.List as L
import qualified Data.Vector.Unboxed as BV
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

    ["test", "deadlock1", chanTypeS, seedS, fuzzyS, elementsS]
      -> do let seed :: Int = read seedS
            let fuzzyNormalizer :: Int = read fuzzyS
            let nrOfElements :: Int = read elementsS
            let chanType = case chanTypeS of
                             "default"
                               -> OneDefaultChan
                             "bounded"
                               -> OneBoundedChan
                             "mvar"
                               -> OneMVarChan
            let producedEementsForSecond :: Int = 5000
            let consumedElementsForSecond :: Int = 500
            let wait s = 10^6 `div` s
            process_manager seed fuzzyNormalizer (wait producedEementsForSecond) (wait consumedElementsForSecond) nrOfElements

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

    ["bench", "fusion1", seedS, vectorDimensionS]
      -> do let seed :: Int = read seedS
            let vectorDimension :: Int = read vectorDimensionS

            let !l = force $ L.take vectorDimension $ randoms $ mkStdGen seed
 
            runMode
              (Run defaultConfig Prefix [""])
              [ bench "list initial warm-up evaluation - not consider" $ nf L.sum l
              , bench "fusion list with foldl" $ nf Optimizations.fusionOnListFoldl l
              , bench "fused list with foldl" $ nf Optimizations.fusedOnListFoldl l
              , bench "fusion list with foldl'" $ nf Optimizations.fusionOnListFoldl' l
              , bench "fused list with foldl'" $ nf Optimizations.fusedOnListFoldl' l
              , bench "fusion list with foldr" $ nf Optimizations.fusionOnListFoldr l
              , bench "fused list with foldr" $ nf Optimizations.fusedOnListFoldr l
              , bench "io-stream" $ nfIO $ Optimizations.fusionOnStream l
              , bench "fused io-stream" $ nfIO $ Optimizations.fusedOnStream l
              ]

            putStrLn $ "fusion list with foldl: " ++ show (Optimizations.fusionOnListFoldl l)
            putStrLn $ "fusion list with foldr: " ++ show (Optimizations.fusionOnListFoldr l)
            putStrLn $ "fused list with foldl: " ++ show (Optimizations.fusedOnListFoldl l)
            putStrLn $ "fused list with foldr: " ++ show  (Optimizations.fusedOnListFoldr l)
            r1 <- Optimizations.fusionOnStream l
            r2 <- Optimizations.fusedOnStream l
            putStrLn $ "io-stream: " ++ show r1
            putStrLn $ "fused io-stream: " ++ show r2

    ["bench", "chan1", seedS, vectorDimensionS]
      -> do let seed :: Int = read seedS
            let vectorDimension :: Int = read vectorDimensionS

            let !l = force $ BV.fromListN vectorDimension $ L.take vectorDimension $ randoms $ mkStdGen seed
 
            runMode
              (Run defaultConfig Prefix [""])
              [ bench "list initial warm-up evaluation - not consider" $ nf (BV.foldl (+) 0) l
              , bench "sumOnVector" $ nf Optimizations.sumOnVector l
              , bench "sumOnStream" $ nfIO $ Optimizations.sumOnStream l
              , bench "sumOnChan" $ nfIO $ Optimizations.sumOnChan l
              , bench "sumOnUnagi" $ nfIO $ Optimizations.sumOnUnagi l
              ]

            r1 <- Optimizations.sumOnStream l
            r2 <- Optimizations.sumOnChan l
            r3 <- Optimizations.sumOnUnagi l

            putStrLn $ "sumOnVector: " ++ show (Optimizations.sumOnVector l)
            putStrLn $ "sumOnStream: " ++ show r1 
            putStrLn $ "sumOnChan: " ++ show r2
            putStrLn $ "sumOnUnagi: " ++ show r3

    ["bench", "task1", seedS, vectorDimensionS]
      -> do let seed :: Int = read seedS
            let vectorDimension :: Int = read vectorDimensionS

            let !l = force $ BV.fromListN vectorDimension $ L.take vectorDimension $ randoms $ mkStdGen seed
 
            runMode
              (Run defaultConfig Prefix [""])
              [ bench "list initial warm-up evaluation - not consider" $ nf (BV.foldl (+) 0) l
              , bench "sumOnChan" $ nfIO $ Optimizations.sumOnChan l
              , bench "sumUsingTasks" $ nfIO $ Optimizations.sumUsingTasks l
              ]

            r1 <- Optimizations.sumOnChan l
            r2 <- Optimizations.sumUsingTasks l

            putStrLn $ "sumOnChan: " ++ show r1
            putStrLn $ "sumUsingTasks: " ++ show r2

    ["test", "buggy-lazy-evaluation"]
      -> testBuggyLazyEvaluation

    _ -> die "Unrecognized options."
