{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Computation where

import Base
import Control.Monad
import qualified Control.Monad.State.Strict as MTL

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

computation2
  :: (Monad m, MonadFree F m)
  => Int
  -> m ()
computation2 n =
  if n == 0
    then return ()
    else do
      computation2 (n-1)
      s <- get
      put $! s + 1

mtlComputation2 :: Int -> MTL.State Int ()
mtlComputation2 n =
  if n == 0
    then return ()
    else do
      mtlComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1
