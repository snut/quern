{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts, FlexibleInstances #-}
{-# Language UndecidableInstances #-}
-- | What if slime-game were based on the d20 srd?

module Slime.Game.D20 where

import Control.Applicative (liftA2)
import Control.Monad.State
import Data.Foldable (fold)
import Data.Bifunctor (first)
import Data.List (sort)
import System.Random

type MonadDice m = (MonadState StdGen m)


data Result = Result { _dice :: [(Int,Int)], _mod :: Int }
  deriving (Eq, Ord, Show, Read)

rolls :: Result -> [Int]
rolls = fmap fst . _dice

roll :: Int -> State StdGen a -> a
roll seed body = evalState body (mkStdGen seed)

instance Semigroup Result where
  a <> b = Result (_dice a <> _dice b) (_mod a + _mod b)
instance Monoid Result where
  mempty = Result [] 0

instance Num Result where
  fromInteger = Result [] . fromInteger
  (+) = (<>)
  a - b = a <> negate b
  a * b = let v = value a in Result (first (* v) <$> _dice b) (_mod b * v)
  negate (Result rs m) = Result (first negate <$> rs) (negate m)
  abs (Result rs m) = Result (first abs <$> rs) (abs m)
  signum (Result rs m) = Result (first signum <$> rs) (signum m)

value :: Result -> Int
value r = sum (rolls r) + _mod r

dX :: MonadDice m => Int -> m Result
dX s
  | s <= 0 = pure $ Result [] 0
  | s == 1 = pure $ Result [(1,1)] 0
  | otherwise = do
    (r, g') <- randomR (1,s) <$> get
    put g'
    pure $ Result [(r,s)] 0

d100, d20, d12, d10, d8, d6, d4 :: MonadDice m => m Result
d100 = dX 100
d20 = dX 20
d12 = dX 12
d10 = dX 10
d8 = dX 8
d6 = dX 6
d4 = dX 4

advantage :: MonadDice m => m Result
advantage = do
  a <- d20
  b <- d20
  if (a >= b)
    then pure a
    else pure b

disadvantage :: MonadDice m => m Result
disadvantage = do
  a <- d20
  b <- d20
  if (a <= b)
    then pure a
    else pure b

keeping :: Int -> Result -> Result
keeping  k (Result rs m) = Result (take k . reverse $ sort rs) m

-- | '4 * d6 `keep` 3' rolls 4 d6 and takes the three highest rolls
keep :: MonadDice m => m Result -> Int -> m Result
keep r k = fmap (keeping k) r
infixl 4 `keep`

instance (MonadDice m) => Num (m Result) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (<>)
  (-) = liftA2 (-)
  n * d = do
    n' <- value <$> n
    fold <$> replicateM n' d
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum

-- tests and checks

newtype DC = DC Int deriving (Eq, Ord, Show, Read)
type Bonus = Int
data Advantage
  = Advantage
  | Normal
  | Disadvantage
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

check :: MonadDice m => Advantage -> DC -> Bonus -> m (Bool, Result)
check adv (DC dc) bonus = do
  d <- case adv of
        Advantage -> advantage
        Normal -> d20
        Disadvantage -> disadvantage
  let r = d { _mod = bonus }
      success = value r >= dc
  pure (success, r)















--
