{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Codensity where

import Control.Applicative
import Control.Monad
import Base
import qualified Free

newtype Codensity f a = Codensity { 
      runCodensity :: forall b. (a -> f b) -> f b 
}

instance Functor (Codensity f) where
    fmap f m = Codensity (\k -> runCodensity m (k. f))

instance Applicative (Codensity f) where
  pure = return
  (<*>) = ap

instance Monad (Codensity f) where
    return a = Codensity (\k -> k a)
    c >>= f  = Codensity (\k -> runCodensity c (\a -> runCodensity (f a) k))

toCodensity :: Monad m => m a -> Codensity m a
toCodensity m = Codensity (m >>=)

fromCodensity :: Monad m => Codensity m a -> m a
fromCodensity c = runCodensity c return 

instance Functor f => MonadFree f (Codensity (Free.Free f)) where
  wrap t = Codensity $ \h -> wrap (fmap (\(Codensity p) -> p h) t)

run :: Codensity (Free.Free F) a -> Int -> (Int, a)
run = Free.run . fromCodensity
