{-# Language RankNTypes #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Slime.Game.LevelGen
  ( generateLevel
  , testLevel
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST
import Linear hiding (E)
import qualified System.Random as R
import Slime.Game.Types

import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

liftR :: Monad m => (g -> (a, g)) -> StateT g m a
liftR = StateT . fmap pure

shuffle :: V.Vector a -> State R.StdGen (V.Vector a)
shuffle xs = do
  xs' <- liftR (shufflePrim xs)
  xs'' <- liftR (shufflePrim xs')
  pure xs''

shufflePrim :: V.Vector a -> R.StdGen -> (V.Vector a, R.StdGen)
shufflePrim xs g
  | V.null xs = (xs, g)
  | otherwise = runST $ flip runStateT g $ do
    v <- lift $ V.thaw xs
    let n = V.length xs
    V.forM_ (V.generate n id) $ \i -> do
      j <- liftR (R.randomR (0, n-1))
      a <- lift $ VM.unsafeRead v i
      b <- lift $ VM.unsafeRead v j
      VM.unsafeWrite v i b
      VM.unsafeWrite v j a
    lift $ V.unsafeFreeze v

newtype LevelGen a = LevelGen
  { _runLevelGen :: ReaderT (V2 Int) (StateT [V2 Int] (State R.StdGen)) a
  } deriving (Functor, Applicative, Monad, MonadState [V2 Int], MonadReader (V2 Int))

liftL :: (R.StdGen -> (a, R.StdGen)) -> LevelGen a
liftL = LevelGen . lift . lift . liftR

runLevelGen :: LevelGen a -> V2 Int -> [V2 Int] -> R.StdGen -> (a, R.StdGen)
runLevelGen (LevelGen body) sz ts g = runState (evalStateT (runReaderT body sz) ts) g

takeTiles :: Int -> LevelGen [V2 Int]
takeTiles n = do
  ts <- get
  let (h,t) = splitAt n ts
  put t
  pure h

randomRot :: LevelGen Float
randomRot = liftL (first fromIntegral . R.randomR d6)
  where d6 = (0,5) :: (Int, Int)

randomScale :: Float -> Float -> LevelGen Float
randomScale l u = liftL (R.randomR (l, u))

genLvlM :: State R.StdGen Level
genLvlM = do
  let sz@(V2 w h) = V2 10 8
      tiles = V.fromList [V2 x y | y <- [0 .. h-1], x <- [0 .. w-1]]
  tiles' <- V.toList <$> shuffle tiles
  liftR (runLevelGen genLvl' sz tiles')

generateLevel :: Int -> Level
generateLevel seed = evalState genLvlM (R.mkStdGen seed)

mkUnit :: FilePath -> Bool -> V2 Int -> LevelGen Unit
mkUnit m pc p = do
  r <- randomRot
  pure $ defaultUnit { _unitMesh = m, _unitPosition = p, _unitRotation = r, _unitPlayerControlled = pc }

mkItem :: FilePath -> V2 Int -> LevelGen Item
mkItem m p = do
  r <- randomRot
  s <- randomScale 0.9 1.2
  pure $ Item{ _itemPosition = p, _itemRotation = r, _itemMesh = m, _itemName = mempty, _itemScale = s}

mkObstacle :: FilePath -> V2 Int -> LevelGen Obstacle
mkObstacle m p = do
  r <- randomRot
  s <- randomScale 0.8 1.3
  pure $ Obstacle{ _obstaclePosition = p, _obstacleRotation = r, _obstacleScale = s, _obstacleMesh = m}

roll :: Int -> Int -> LevelGen Int
roll l u = liftL (R.randomR (l, u))

generate :: Int -> Int -> (V2 Int -> LevelGen a) -> LevelGen [a]
generate l u f = roll l u >>= takeTiles >>= traverse f

genLvl' :: LevelGen Level
genLvl' = do
  -- slimes
  pinks <- generate 1 2 (mkUnit "pink_slime" True)
  wizards <- generate 1 2 (mkUnit "wiz_slime" True)
  greens <- generate 1 3 (mkUnit "gcube" False)
  -- plants
  clover <- generate 6 10 (mkItem "clover")
  mushes <- generate 3 5 (mkItem "mushroom_cluster")
  -- things
  stumps <- generate 4 7 (mkObstacle "stump")
  rocks <- generate 3 7 (mkObstacle "jagged_rock")
  rocks2 <- generate 3 6 (mkObstacle "rock_vclr")
  piles <- generate 4 9 (mkObstacle "rock_pile")
  crystals <- generate 2 5 (mkObstacle "crystal")
  sz <- ask
  pure $ Level
    { _levelUnits = M.fromList $ zip (E <$> [1..]) (pinks <> wizards <> greens )
    , _levelItems = V.fromList (clover <> mushes)
    , _levelObstacles = V.fromList (rocks <> piles <> crystals <> rocks2 <> stumps)
    , _levelSize = sz
    }

testLevel :: Level
testLevel = generateLevel 0x1234
