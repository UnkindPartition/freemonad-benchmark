{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Main (main) where

import Base
import qualified Free
import qualified Church
import qualified Codensity
import qualified NoRemorse
import Control.Monad
import qualified Control.Monad.State.Strict as MTL

import Criterion (bench, nf, bgroup, Benchmark)
import Criterion.Main (defaultMain)
import qualified StateF
import qualified Coroutine

computation
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- get
  put $! s + 1

mtlComputation :: Int -> MTL.State Int ()
mtlComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

computationF
  :: (Monad m, MonadFree StateF.StateF m)
  => Int
  -> m ()
computationF n = forM_ [1..n] $ \_ -> do
  s <- StateF.get
  StateF.put $! s + 1

computationCor
  :: Int
  -> Coroutine.CorState ()
computationCor n = forM_ [1..n] $ \_ -> do
  s <- Coroutine.get
  Coroutine.put $! s + 1

computationCorCod
  :: Int
  -> Codensity.Codensity Coroutine.CorState ()
computationCorCod n = forM_ [1..n] $ \_ -> do
  s <- Coroutine.getC
  Coroutine.putC $! s + 1

computation2
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation2 n =
  if n == 0
    then return ()
    else do
      s <- get
      put $! s + 1
      computation2 (n-1)

mtlComputation2 :: Int -> MTL.State Int ()
mtlComputation2 n =
  if n == 0
    then return ()
    else do
      s <- MTL.get
      MTL.put $! s + 1
      mtlComputation2 (n-1)

n = 200
n2 = 5

benchmarks
  :: (forall m . (Monad m, MonadFree F m) => Int -> m ())
  -> (Int -> MTL.State Int ())
  -> Int
  -> [Benchmark]
benchmarks computation mtlComputation n =
  [ bench "Free" $ nf (flip Free.run 0 . computation) n
  , bench "Free/lazy" $ nf (flip Free.runLazily 0 . computation) n
  , bench "Chruch" $ nf (flip Church.run 0 . computation) n
  , bench "Codensity" $ nf (flip Codensity.run 0 . computation) n
  , bench "NoRemorse" $ nf (flip NoRemorse.run 0 . computation) n
  , bench "MTL" $ nf (flip MTL.runState 0 . mtlComputation) n
  , bench "StateF/Free" $ nf (flip StateF.run 0 . computationF) n
  , bench "StateF/Codensity" $ nf (flip StateF.runCod 0 . computationF) n
  , bench "Coroutine" $ nf (flip Coroutine.run 0 . computationCor) n
  , bench "Coroutine/Codensity" $ nf (flip Coroutine.runCod 0 . computationCorCod) n
  ]

main :: IO ()
main = defaultMain
  [ bgroup "Linear" $ benchmarks computation mtlComputation n
  -- , bgroup "Tree" $ benchmarks computation2 mtlComputation2 n2
  ]
