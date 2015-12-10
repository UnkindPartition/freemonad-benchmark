-- http://okmij.org/ftp/Haskell/extensible/more.pdf
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Freer where
import Base
import Control.Monad

data Free f a
  = Pure a
  | forall b . Impure (f b) (b -> Free f a)

instance Functor (Free f) where
  fmap = liftM
instance Applicative (Free f) where
  pure = return
  (<*>) = ap
instance Monad (Free f) where
  return = Pure
  Pure x >>= k = k x
  Impure a k1 >>= k = Impure a (k1 >=> k)

fold :: (forall b . f b -> b) -> Free f a -> a
fold _ (Pure x) = x
fold f (Impure a k) =  fold f (k (f a))

instance Functor f => MonadFree f (Free f) where
  wrap a = Impure a id

run :: Free F a -> Int -> (Int, a)
run (Pure x) s = (s, x)
run (Impure (F a) k) s =
  case a s of
    (s', x) -> run (k x) s'
