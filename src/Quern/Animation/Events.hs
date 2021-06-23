module Quern.Animation.Events
  ( Events
  , fromUnsortedEvents
  , singletonEvent
  , insertEvent
  , shiftEvents
  , sampleEvents
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as VA
import Data.Foldable (forM_)
import Data.Function (on)
import Control.Monad.ST

data Events e = Events
  { _eventTimestamps :: !(VU.Vector Double)
  , _eventPayloads :: !(V.Vector e)
  } deriving (Eq, Show)

instance Functor Events where
  fmap f (Events ts ps) = Events ts (fmap f ps)

instance Semigroup (Events e) where (<>) = mergeEvents
instance Monoid (Events e) where mempty = Events mempty mempty

fromUnsortedEvents :: [(Double, e)] -> Events e
fromUnsortedEvents tsps = Events ts ps
  where
    vt = VU.fromList (fst <$> tsps)
    vp = V.fromList (snd <$> tsps)
    (ts, ps) = eventSort vt vp mempty mempty

mergeEvents :: Events e -> Events e -> Events e
mergeEvents ea@(Events ats aps) eb@(Events bts bps)
  | VU.null ats = eb
  | VU.null bts = ea
  | otherwise = Events ts ps
  where
    (ts, ps) = eventSort ats aps bts bps

eventSort :: VU.Vector Double -> V.Vector a -> VU.Vector Double -> V.Vector a -> (VU.Vector Double, V.Vector a)
eventSort ats aps bts bps = (V.convert ts, ps)
  where
    an = VU.length ats
    bn = VU.length bts
    (ts, ps) = V.unzip tps
    tps = runST $ do
            tsps <- VM.unsafeNew (an + bn)
            forM_ [0..an-1] $ \i -> VM.unsafeWrite tsps i (VU.unsafeIndex ats i, V.unsafeIndex aps i)
            forM_ [0..bn-1] $ \i -> VM.unsafeWrite tsps (i+an) (VU.unsafeIndex bts i, V.unsafeIndex bps i)
            VA.sortBy (compare `on` fst) tsps
            V.unsafeFreeze tsps

singletonEvent :: e -> Double -> Events e
singletonEvent e t = Events (VU.singleton t) (V.singleton e)

-- TODO: this is not smart
insertEvent :: e -> Double -> Events e -> Events e
insertEvent e t = mergeEvents (singletonEvent e t)

shiftEvents :: Double -> Events e -> Events e
shiftEvents delta es@(Events ts ps)
  | delta == 0 = es
  | otherwise = Events (VU.map (+ delta) ts) ps

sampleEvents :: Double -> Double -> Events e -> V.Vector (Double, e)
sampleEvents from dur (Events ts es) = V.zip (V.convert ts') es'
  where
    ts' = if ix1 <= ix0 then mempty else VU.slice ix0 (ix1 - ix0) ts
    es' = if ix1 <= ix0 then mempty else V.slice ix0 (ix1 - ix0) es
    ix0 = VU.foldl' (\ix t -> if t < from then succ ix else ix) 0 ts
    unto = from + dur
    ix1 = VU.foldl' (\ix t -> if t <= unto then succ ix else ix) 0 ts
