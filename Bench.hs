{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Base
import qualified Free
import qualified Church
import qualified Codensity
import qualified NoRemorse
import Control.Monad
import qualified Control.Monad.State.Strict as MTL

import Criterion (bench, nf)
import Criterion.Main (defaultMain)

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

n = 20

main :: IO ()
main = defaultMain
  [ bench "Free" $ nf (flip Free.run 0 . computation) n
  , bench "Free/lazy" $ nf (flip Free.runLazily 0 . computation) n
  , bench "Chruch" $ nf (flip Church.run 0 . computation) n
  , bench "Codensity" $ nf (flip Codensity.run 0 . computation) n
  , bench "NoRemorse" $ nf (flip NoRemorse.run 0 . computation) n
  , bench "MTL" $ nf (flip MTL.runState 0 . mtlComputation) n
  ]
