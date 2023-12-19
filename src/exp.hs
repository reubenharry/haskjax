{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}


module Exp where

import Linear.V ( dim, fromVector, V(V), Dim )
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Linear ((^*), (*^), Additive ((^+^)), Metric (norm, dot, signorm), (^/))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat)
import Control.Monad.Representable.Reader (tabulate, MonadReader (reader))
import Numeric.AD (grad)
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad ((<=<))
import Data.MonadicStreamFunction (MSF, arrM)
import Control.Monad.Bayes.Class (MonadDistribution(..))
import Control.Monad.Bayes.Sampler.Strict ( SamplerIO, sampleIO )

data PhaseSpace (n :: Nat) a where
  Point :: {position :: V n a, momentum :: V n a} -> PhaseSpace n a
  deriving (Show, Eq)

type Operator dim = PhaseSpace dim Double -> SamplerIO (PhaseSpace dim Double)

type DensityFunction = (forall t a . (Foldable t, Functor t, Floating a) => t a -> a)

momentumUpdate :: forall dim . KnownNat dim => DensityFunction -> Double -> Operator dim
momentumUpdate logdensity stepSize (Point x u) = pure $ Point x uu
    where
    g = grad logdensity x  :: V dim Double
    g_norm = norm g
    d = fromIntegral $ dim g
    e = negate $ g ^/ g_norm
    delta = stepSize * g_norm / (d-1)
    uu = (u + e^*(sinh delta + dot e (u ^* ( cosh delta -1)))) ^/ (   cosh delta  + dot e (u ^* sinh delta))


positionUpdate :: KnownNat dim => Double -> Operator dim
positionUpdate e (Point x u) = pure $ Point (x ^+^ e *^ u) u

leapfrog :: (Double -> Operator dim) -> (Double -> Operator dim) -> Operator dim
leapfrog o1 o2 = o2 0.5 <=< o1 1 <=< o2 0.5

maruyama :: forall n m . (KnownNat n, MonadDistribution m) => Double -> Double -> PhaseSpace n Double -> m (PhaseSpace n Double)
maruyama l stepSize (Point x u) = Point x <$> (partialRefresh stepSize l (u :: V n Double))

partialRefresh :: (KnownNat n, MonadDistribution f) => Double -> Double -> V n Double -> f (V n Double)
partialRefresh stepSize l momentum = signorm . (^+^ momentum) <$> z
    where
        d = fromIntegral $ dim momentum
        nu = sqrt ((exp (2 * stepSize / l) - 1.0) / d)
        z = (fmap . fmap) (nu *) (mapM (const (normal 0 1)) momentum)

generateUnitVector :: Int -> SamplerIO (V 10 Double)
generateUnitVector dim =  signorm . V <$> Vec.replicateM dim (normal 0 1)


integrate :: (Operator dim, Double -> Operator dim , Double -> Operator dim ) -> [Double] -> Operator dim
integrate (noise, p, v) sequence = noise  <=< foldr (<=<) pure steps
    where
        steps = zipWith id (cycle [p,v]) (mirror sequence)
        mirror x = x ++ reverse x

integrateWith :: Operator dim -> Int -> Operator dim
integrateWith o i = foldr (<=<) pure $ replicate i o


sampler :: forall dim. KnownNat dim => DensityFunction -> Double -> Double -> Int -> V dim Double -> IO (PhaseSpace dim Double)
sampler logDensity l stepSize numSteps initialPosition  = sampleIO $ integrateWith integrator numSteps initialState
    where
          integrator = integrate (maruyama l stepSize, positionUpdate, momentumUpdate logdensity) (fmap (*stepSize) [1, 0.5])

          initialMomentum = (V $ Vec.fromList [1.0, 1.0]) :: V dim Double
          initialState = Point initialPosition initialMomentum


logdensity :: DensityFunction
logdensity x = negate (0.5 * sum (fmap (**2) x))

ones :: V 2 Double
ones = fromMaybe (error "dimension mismatch") $ fromVector $ Vec.fromList [1.0,1.0] :: V 2 Double

example :: IO String
example = show <$> sampler logdensity 1 1e-3 10 ones


