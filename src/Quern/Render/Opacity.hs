{-# Language DeriveGeneric #-}

module Quern.Render.Opacity where


import GHC.Generics (Generic)

data Opacity
  = Opaque
  | Transparent
  | Masked
  | Additive
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic)
