{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Base
import qualified Free
import qualified Church
import qualified Codensity
import qualified NoRemorse
import Control.Monad

import Criterion (bench, nf)
import Criterion.Main (defaultMain)

computation
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- get
  put $! s + 1

n = 20

main :: IO ()
main = defaultMain
  [ bench "Free" $ nf (flip Free.run 0 . computation) n
  , bench "Free/lazy" $ nf (flip Free.runLazily 0 . computation) n
  , bench "Chruch" $ nf (flip Church.run 0 . computation) n
  , bench "Codensity" $ nf (flip Codensity.run 0 . computation) n
  , bench "NoRemorse" $ nf (flip NoRemorse.run 0 . computation) n
  ]
