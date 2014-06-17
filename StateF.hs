{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module StateF where

import Base hiding (put,get)
import Codensity hiding (run)
import Free hiding (run)

data StateF k where
  Get :: (Int -> k) -> StateF k
  Put :: Int -> k -> StateF k
  deriving Functor

get :: MonadFree StateF free => free Int
get = wrap (Get return)

put :: MonadFree StateF free => Int -> free ()
put s = wrap (Put s (return ()))

run :: Free StateF a -> Int -> (Int , a)
run (Pure a)         s = (s,a)
run (Free (Get k))   s = run (k s) s
run (Free (Put s k)) _ = run k s

runCod :: Codensity (Free StateF) a -> Int -> (Int,a)
runCod = run . fromCodensity
