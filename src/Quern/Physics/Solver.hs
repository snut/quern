{-| This module does noooothing -}
module Quern.Physics.Solver where

import Linear
import Data.Foldable

data BodyConstants = BodyConstants
  { _invMass :: !Float
  , _invInertia :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)

data Body = Body
  { _bodyPosition :: !(V3 Float) -- ^ Centre of mass
  , _bodyRotation :: !(Quaternion Float)
  , _bodyLinVelocity :: !(V3 Float) -- ^ Velocity at centre of mass
  , _bodyAngularVelocity :: !(V3 Float)
  } deriving (Eq, Ord, Show, Read)
