{-# LANGUAGE GADTs #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
module Exp where

import Linear.V hiding (Dim)
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Linear ((^*), (*^), Additive ((^+^)), Metric (norm, dot), (^/))
import qualified Data.Vector as Vec
import GHC.TypeLits (Nat, KnownNat)
import Data.Data (Proxy)
import Control.Monad.Representable.Reader (tabulate)
import Numeric.AD (grad)

data PhaseSpace (n :: Nat) a where
  Point :: {position :: V n a, momentum :: V n a} -> PhaseSpace n a
  deriving (Show, Eq)

type Dim = 2
type Operator dim = PhaseSpace dim Double -> PhaseSpace dim Double

type DensityFunction = (forall t a . (Foldable t, Functor t, Floating a) => t a -> a)

momentumUpdate :: forall dim . KnownNat dim => DensityFunction -> Double -> Operator dim
momentumUpdate logdensity stepSize (Point x u) = Point x uu
    where
    g = grad logdensity x  :: V dim Double
    g_norm = norm g
    d = fromIntegral $ dim g
    e = - g ^/ g_norm
    delta = stepSize * g_norm / (d-1)
    uu = (u + e^*(sinh delta + dot e (u ^* ( cosh delta -1)))) ^/ (   cosh delta  + dot e (u ^* sinh delta))


positionUpdate :: KnownNat dim => Double -> Operator dim
positionUpdate e (Point x u) = Point (x ^+^ e *^ u) u

leapfrog :: (Double -> Operator dim) -> (Double -> Operator dim) -> Operator dim
leapfrog o1 o2 = o2 0.5 . o1 1 . o2 0.5

integrate :: (Double -> Operator dim , Double -> Operator dim ) -> [Double] -> Operator dim
integrate (p, v) sequence = foldr (.) id steps
    where
        steps = zipWith id (cycle [p,v]) (mirror sequence)
        mirror x = x ++ reverse x

integrateWith :: Operator dim -> Int -> Operator dim
integrateWith o i = foldr (.) id $ replicate i o

noise :: Operator dim
noise = undefined

sampler :: DensityFunction -> p2 -> Double -> Int -> p4 -> PhaseSpace Dim Double
sampler logDensity l stepSize numSteps initialPosition  = integrateWith integrator numSteps initialState
    where
          integrator = integrate (positionUpdate, momentumUpdate logdensity) (fmap (*stepSize) [1, 0.5])
          initialPos = (V $ Vec.fromList [1.0, 1.0]) :: V Dim Double
          initialMomentum = (V $ Vec.fromList [1.0, 1.0]) :: V Dim Double
          initialState = Point initialPos initialMomentum


logdensity :: DensityFunction
logdensity x = negate (0.5 * sum (fmap (**2) x))


b :: String
b = show $ sampler logdensity undefined 1e-3 10 undefined
