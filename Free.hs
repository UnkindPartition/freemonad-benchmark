{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Free where

import Base
import Control.Applicative

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)
  {-# INLINE fmap #-}

instance Functor f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  return = Pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

instance Functor f => MonadFree f (Free f) where wrap = Free

run :: Free F a -> Int -> (Int, a)
run (Pure x) s = (s, x)
run (Free (F a)) s =
  case a s of
    (s', a') -> run a' s'

runLazily :: Free F a -> Int -> (Int, a)
runLazily (Pure x) s = (s, x)
runLazily (Free (F a)) s =
  case a s of
    ~(s', a') -> run a' s'
