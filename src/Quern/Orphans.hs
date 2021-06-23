{-# OPTIONS_GHC -Wno-orphans #-}

{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveAnyClass #-}
module Quern.Orphans where

import Data.Aeson as A
--import Data.Flat as F
import Data.Persist as P
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Data.Word
import Foreign (Storable)
import Foreign.C.Types
import Linear
import Numeric.Half

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON (Object o) = do
    x <- o .: "x"
    y <- o .: "y"
    pure $ V2 x y
  parseJSON _ = mempty

instance (ToJSON a) => ToJSON (V2 a) where
  toJSON (V2 x y) = object ["x" .= x, "y" .= y]
  toEncoding (V2 x y) = pairs ("x" .= x <> "y" .= y)

instance (FromJSON a) => FromJSON (V3 a) where
  parseJSON (Object o) = do
    x <- o .: "x"
    y <- o .: "y"
    z <- o .: "z"
    pure $ V3 x y z
  parseJSON _ = mempty

instance (ToJSON a) => ToJSON (V3 a) where
  toJSON (V3 x y z) = object ["x" .= x, "y" .= y, "z" .= z]
  toEncoding (V3 x y z) = pairs ("x" .= x <> "y" .= y <> "z" .= z)

instance (FromJSON a) => FromJSON (V4 a) where
  parseJSON (Object o) = do
    x <- o .: "x"
    y <- o .: "y"
    z <- o .: "z"
    w <- o .: "w"
    pure $ V4 x y z w
  parseJSON _ = mempty

instance (ToJSON a) => ToJSON (V4 a) where
  toJSON (V4 x y z w) = object ["x" .= x, "y" .= y, "z" .= z, "w" .= w]
  toEncoding (V4 x y z w) = pairs ("x" .= x <> "y" .= y <> "z" .= z <> "w" .= w)


instance (Storable a, Persist a) => Persist (VS.Vector a) where
  put v = do
    put (VS.length v)
    VS.mapM_ put v
  get = do
    n <- get
    VS.replicateM n get

instance (Persist a) => Persist (V.Vector a) where
  put v = do
    put (V.length v)
    V.mapM_ put v
  get = do
    n <- get
    V.replicateM n get

-- replace with coerces
w16 :: CUShort -> Word16
w16 = fromIntegral
cus :: Word16 -> CUShort
cus = fromIntegral

instance Persist Half where
  put (Half x) = put (w16 x)
  get = Half . cus <$> get

deriving instance Persist a => Persist (V1 a)
deriving instance Persist a => Persist (V2 a)
deriving instance Persist a => Persist (V3 a)
deriving instance Persist a => Persist (V4 a)


{-
instance (Storable a, Flat a) => Flat (VS.Vector a) where
  size v tot = VS.foldl' (\sz a -> size a sz) (size (VS.length v) tot) v
  encode v = F.encode (VS.length v) <> (VS.foldl' (\m a -> m <> F.encode a) mempty v)
  decode = do
    count <- F.decode
    VS.replicateM count F.decode

instance (Flat a) => Flat (V.Vector a) where
  size v tot = V.foldl' (\sz a -> size a sz) (size (V.length v) tot) v
  encode v = F.encode (V.length v) <> (V.foldl' (\m a -> m <> F.encode a) mempty v)
  decode = do
    count <- F.decode
    V.replicateM count F.decode


instance Flat Half where
  size (Half x) = size (w16 x)
  encode (Half x) = F.encode (w16 x)
  decode = Half . cus <$> F.decode
deriving instance Flat a => Flat (V2 a)
deriving instance Flat a => Flat (V3 a)
deriving instance Flat a => Flat (V4 a)
-}
