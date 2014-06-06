{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Church where

import Control.Applicative
import Control.Monad
import Base

newtype ChurchFree f a = ChurchFree { runChurchFree :: forall w. (a -> w) -> (f w -> w) -> w }

instance Functor (ChurchFree f) where
    fmap = liftM
    {-# INLINE fmap #-}

instance Applicative (ChurchFree f) where
    pure = return
    (<*>) = ap

instance Monad (ChurchFree f) where
    return x = ChurchFree $ \ret _ -> ret x
    {-# INLINE return #-}

    m >>= f = ChurchFree $ \ret emb -> runChurchFree m (\v -> runChurchFree (f v) ret emb) emb
    {-# INLINE (>>=) #-}

instance Functor f => MonadFree f (ChurchFree f) where
  wrap a = ChurchFree $ \ret w -> w (fmap (\x -> runChurchFree x ret w) a)

run :: ChurchFree F a -> Int -> (Int, a)
run a =
  runChurchFree a
    (\r s -> (s, r))
    (\t s -> case (unF t s) of (s', k) -> k s')
