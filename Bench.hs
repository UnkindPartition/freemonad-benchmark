{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Base
import qualified Free
import qualified Church
import qualified Codensity
import Control.Monad

import Criterion (bench, whnf)
import Criterion.Main (defaultMain)

computation
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- get
  put $! s + 1

main :: IO ()
main = defaultMain
  [ bench "Free" $ whnf (flip Free.run 0 . computation) 20
  , bench "Free/lazy" $ whnf (flip Free.runLazily 0 . computation) 20
  , bench "Chruch" $ whnf (flip Church.run 0 . computation) 20
  , bench "Codensity" $ whnf (flip Codensity.run 0 . computation) 20
  ]
