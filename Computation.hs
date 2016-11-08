{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Computation where

import Base
import Control.Monad
import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as MTL
import qualified Fused

{- It is only fair to give the computations that use a free monad the same
advantage as MTL, namely that they become specialized to the concrete monad
in question. Indeed, the results for msComputation+MTL.State below (for
which there is no SPECIALIZE pragma) demonstrate that this makes MTL a lot
slower.

This does not help much for most of the free monad implementations. But it
gives huge wins for Fused (and MTL).

Specialization can be done manually, as with mtlComputation and fusedComputation
below. Or you can let the compiler do it with a SPECIALIZE pragma, see e.g. the
one for msComputation+Fused below.
-}

computation :: MonadFree F m => Int -> m ()
computation n = forM_ [1..n] $ \_ -> do
  s <- Base.get
  Base.put $! s + 1

msComputation :: MonadState Int m => Int -> m ()
msComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1
{-# SPECIALIZE msComputation :: Int -> Fused.Codensity Fused.H () #-}

mtlComputation :: Int -> MTL.State Int ()
mtlComputation n = forM_ [1..n] $ \_ -> do
  s <- MTL.get
  MTL.put $! s + 1

fusedComputation :: Int -> Fused.Codensity Fused.H ()
fusedComputation n = forM_ [1..n] $ \_ -> do
  s <- Base.get
  Base.put $! s + 1

computation2 :: MonadFree F m => Int -> m ()
computation2 n =
  if n == 0
    then return ()
    else do
      computation2 (n-1)
      s <- Base.get
      Base.put $! s + 1

msComputation2 :: MonadState Int m => Int -> m ()
msComputation2 n =
  if n == 0
    then return ()
    else do
      msComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1
{-# SPECIALIZE msComputation2 :: Int -> Fused.Codensity Fused.H () #-}

mtlComputation2 :: Int -> MTL.State Int ()
mtlComputation2 n =
  if n == 0
    then return ()
    else do
      mtlComputation2 (n-1)
      s <- MTL.get
      MTL.put $! s + 1

fusedComputation2 :: Int -> Fused.Codensity Fused.H ()
fusedComputation2 n =
  if n == 0
    then return ()
    else do
      computation2 (n-1)
      s <- Base.get
      Base.put $! s + 1
