{- Idea and implementation are from the paper "Fusion for Free: Efficient
Algebraic Effect Handlers" (MPC 2015) by Nicolas Wu and Tom Schrijvers.

Their class 'TermMonad' is already present here under the name
'Base.MonadFree' (modulo swapped type parameters and class method 'con'
renamed 'wrap'). For simplicity, we skip the 'TermAlgebra' class in the
paper and implement the instance for 'MonadFree' directly; this also avoids
the need for UndecidableInstances and other type class unpleasantness.

Since this is only a demo of an efficient free monad implementation and not
an extensible effects library, we don't bother to parameterize the carrier
functor with a base monad, thus further simplifying the code.

The fused representation is the Codensity transform of the carrier functor.

While the authors develop their code starting with the standard free modad
type (exactly as in module Free), it disappears completely after fusion and
is thus not present here.

The correctness of the fusion crucially relies on the stateful computation
(what is passed to 'run') only using the polymorphic API exposed by the
'MonadFree' class.

However, to get decent speed, it is equally crucial that the computation is
specialized to the concrete 'Codensity H' monad. This can be done either
manually or with a compiler pragma. See module Computation for details.
-}

{-# LANGUAGE RankNTypes #-} -- to enable 'forall' keyword
module Fused (run, Codensity, H) where

import Base

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class

-- The state carrier functor; it coincides with F, but using F here instead of
-- H is a lot slower (at least factor 5). I have not yet found out why.
newtype H a = H { unH :: Int -> (Int, a) }

run :: Codensity H a -> Int -> (Int, a)
run = unH . flip runCodensity genH

genH :: a -> H a
genH x = H $ \s -> (s, x)

algF :: F (H a) -> H a
algF (F f) = H $ \s -> case f s of (s', g) -> unH g s'

-- generic API exactly as for MTL, only needed for msComputation
instance MonadState Int (Codensity H) where
  {-# SPECIALIZE instance MonadState Int (Codensity H) #-}
  get = wrap (F (\s -> (s, return s)))
  put s = wrap (F (\_ -> (s, return ())))

instance MonadFree F (Codensity H) where
  wrap = alg_cod algF

alg_cod :: Functor f => (forall x. f (h x) -> h x) -> (f (Codensity h a) -> Codensity h a)
alg_cod alg = \op -> Codensity (\k -> alg (fmap (flip runCodensity k) op))

-- Could as well use Control.Monad.Copdensity from kan-extensions, except
-- that it has instances that overlap with the one for MonadState above.

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
