{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Main (main) where

import Base
import Computation
import qualified Free
import qualified Church
import qualified Codensity
import qualified NoRemorse
import qualified Freer
import qualified Fused
import Control.Monad
import qualified Control.Monad.State.Strict as MTL

import Criterion (bench, nf, bgroup, Benchmark)
import Criterion.Main (defaultMain)

n = 50

{-
-- Passing computations with higher rank 'forall m...'
-- types destroys specialization!
-- This makes Fused/monadstate as slow as Fused/generic.

type Comp m = Int -> m ()

benchmarks :: (forall m. MonadFree F m => Comp m)
           -> (forall m. MTL.MonadState Int m => Comp m)
           -> Comp (Fused.Codensity Fused.H)
           -> Comp (MTL.State Int)
           -> [Benchmark]
benchmarks generic monadstate fused mtl = 
  [ bench "Free strict/generic" $ run_bench (Free.run . generic) n
  , bench "Free lazy/generic" $ run_bench (Free.runLazily . generic) n
  , bench "Church/generic" $ run_bench (Church.run . generic) n
  , bench "Codensity/generic" $ run_bench (Codensity.run . generic) n
  , bench "NoRemorse/generic" $ run_bench (NoRemorse.run . generic) n
  , bench "Freer/generic" $ run_bench (Freer.run . generic) n
  , bench "Fused/generic" $ run_bench (Fused.run . generic) n
  , bench "Fused/monadstate" $ run_bench (Fused.run . monadstate) n
  , bench "Fused/specialized" $ run_bench (Fused.run . fused) n
  , bench "MTL/generic" $ run_bench (MTL.runState . monadstate) n
  , bench "MTL/specialized" $ run_bench (MTL.runState . mtl) n
  ]
-}

run_bench comp = nf $ \n -> comp n 0

main :: IO ()
main = defaultMain
  [ bgroup "Right-assoc" $
    -- benchmarks computation msComputation fusedComputation mtlComputation
    [ bench "Free strict/generic" $ run_bench (Free.run . computation) n
    , bench "Free lazy/generic" $ run_bench (Free.runLazily . computation) n
    , bench "Church/generic" $ run_bench (Church.run . computation) n
    , bench "Codensity/generic" $ run_bench (Codensity.run . computation) n
    , bench "NoRemorse/generic" $ run_bench (NoRemorse.run . computation) n
    , bench "Freer/generic" $ run_bench (Freer.run . computation) n
    , bench "Fused/generic" $ run_bench (Fused.run . computation) n
    , bench "Fused/monadstate" $ run_bench (Fused.run . msComputation) n
    , bench "Fused/specialized" $ run_bench (Fused.run . fusedComputation) n
    , bench "MTL/generic" $ run_bench (MTL.runState . msComputation) n
    , bench "MTL/specialized" $ run_bench (MTL.runState . mtlComputation) n
    ]
  , bgroup "Left-assoc" $
    -- benchmarks computation2 msComputation2 fusedComputation2 mtlComputation2
    [ bench "Free strict/generic" $ run_bench (Free.run . computation2) n
    , bench "Free lazy/generic" $ run_bench (Free.runLazily . computation2) n
    , bench "Church/generic" $ run_bench (Church.run . computation2) n
    , bench "Codensity/generic" $ run_bench (Codensity.run . computation2) n
    , bench "NoRemorse/generic" $ run_bench (NoRemorse.run . computation2) n
    , bench "Freer/generic" $ run_bench (Freer.run . computation2) n
    , bench "Fused/generic" $ run_bench (Fused.run . computation2) n
    , bench "Fused/monadstate" $ run_bench (Fused.run . msComputation2) n
    , bench "Fused/specialized" $ run_bench (Fused.run . fusedComputation2) n
    , bench "MTL/generic" $ run_bench (MTL.runState . msComputation2) n
    , bench "MTL/specialized" $ run_bench (MTL.runState . mtlComputation2) n
    ]
  ]
