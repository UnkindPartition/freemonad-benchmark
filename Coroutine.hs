{-# LANGUAGE GADTs #-}

module Coroutine where

import Codensity hiding (run)

data Coroutine f a where
  Ret :: a -> Coroutine f a
  Ask :: f o -> (o -> Coroutine f a) -> Coroutine f a

instance Functor (Coroutine f) where
  fmap f (Ret x)    = Ret (f x)
  fmap f (Ask op k) = Ask op (fmap f . k)

instance Monad (Coroutine f) where
  return         = Ret
  Ret x    >>= f = f x
  Ask op k >>= f = Ask op (\o -> k o >>= f)

data StateO o where
  Get :: StateO Int
  Put :: Int -> StateO ()

type CorState = Coroutine StateO

get :: CorState Int
get = Ask Get return

put :: Int -> CorState ()
put s = Ask (Put s) return

run :: CorState a -> Int -> (Int,a)
run (Ret x)         s = (s,x)
run (Ask Get k)     s = run (k s)  s
run (Ask (Put s) k) _ = run (k ()) s

getC :: Codensity CorState Int
getC = Codensity (Ask Get)

putC :: Int -> Codensity CorState ()
putC s = Codensity (Ask (Put s))

runCod :: Codensity CorState a -> Int -> (Int,a)
runCod = run . fromCodensity
