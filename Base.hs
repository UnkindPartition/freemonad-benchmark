{-# LANGUAGE DeriveFunctor, FunctionalDependencies, FlexibleContexts #-}
module Base where

data F a = F { unF :: Int -> (Int, a) }
  deriving Functor

get :: MonadFree F free => free Int
get = wrap . F $ \s -> (s, return s)

put :: MonadFree F free => Int -> free ()
put s = wrap . F $ \_ -> (s, return ())

class Monad m => MonadFree f m | m -> f where
  wrap :: f (m a) -> m a
