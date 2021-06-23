{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Quern.UI.Widget where

import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Maybe (isNothing)
import Quern.Types

import Linear
--import SDL.Event as SDL

import Data.IORef
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IM
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens ()

--import Quern.Input
import Quern.Physics.AxisAlignedBox
import Data.Functor.Foldable
import Data.Functor.Classes (Show1(..))
import Data.Fix (Fix(..))


-- Ugh. Widgets as trees?
-- can be cata'd into some stateful monad I guess

-- horizontal and vertical splits are a useful alignment thing
data Split
  = FixedFirst !Int
  | FixedSecond !Int
  | SplitRatio !Float
  deriving (Eq,Ord,Show,Read)

data Dir = H | V deriving (Eq, Ord, Bounded, Enum, Show, Read)

newtype Rect = Rect (AABox V2 Int)
  deriving (Eq, Ord, Show, Read)

newtype Size = Size (V2 Int)
  deriving (Eq, Ord, Show, Read)

-- hmm, sizes
data WidgetF a r
  = TextF Text
--  | ImageF String
--  | SplitF Dir Split r r
  | BoxF Dir r [r] -- NonEmpty r?
  | ButtonF Size a r
  | PanelF Int r
  | EmptyF
  deriving (Functor)

instance Show a => Show1 (WidgetF a) where
  liftShowsPrec s ss lvl w = showParen (lvl > 10) $ case w of
      TextF t ->  showString "Text " . showsPrec 0 t
      --ImageF p -> showString "Image " . showsPrec 0 p
      BoxF d r rs -> showString "Box " . showsPrec 0 d . spc . ss  (r:rs)
      ButtonF (Size sz) a r -> showString "Button " . showsPrec 11 sz . spc . showsPrec 0 a . spc . s 0 r
      PanelF b r -> showString "Panel " . showsPrec 0 b . spc . s 0 r
      EmptyF -> showString "Empty"
    where
      spc = showString " "

type Widget a = Fix (WidgetF a)

-- semigroup composition defaults to horizontal
instance Semigroup (Widget a) where
  a <> b = fitBox H a [b]
instance Monoid (Widget a) where
  mempty = Fix EmptyF

text :: Text -> Widget a
text = Fix . TextF



--image :: String -> Widget a
--image = Fix . ImageF

button :: V2 Int -> a -> Widget a -> Widget a
button sz clicked = Fix . ButtonF (Size sz) clicked

panel :: Int -> Widget a -> Widget a
panel border = Fix . PanelF border

fitBox :: Dir -> Widget a -> [Widget a] -> Widget a
fitBox d a = Fix . BoxF d a

gtest :: a -> Widget a
gtest x = panel 2 $ button (V2 128 32) x $ fitBox H (text "OK") []

guiSize :: Widget a -> AABox V2 Int
guiSize = cata alg
  where
    alg EmptyF = degenerate 0
    alg (TextF t) = mkAABox 0 $ V2 (T.length t * 8) 16 -- lies
    --alg (ImageF _) = mkAABox 0 64 -- damn lies
    alg (BoxF d r rs) = stacking d r rs
    alg (ButtonF (Size sz) _ r) = mkAABox 0 sz <> expand 2 r -- margin
    alg (PanelF border r) = grow (max 0 border) r


stacking :: (Num a, Ord a, Foldable f) => Dir -> AABox V2 a -> f (AABox V2 a) -> AABox V2 a
stacking d r@(AABox mn@(V2 mnX mnY) _) rs = case d of
    H -> mkAABox mn (V2 (mnX + maxW) minH)
    V -> mkAABox mn (V2 minW (mnY + maxH))
  where
    V2 maxW maxH = foldl' (\a b -> size b + a) (size r) rs
    V2 minW minH = foldl' (\a b -> max <$> size b <*> a) (size r) rs


guiClick :: Widget a -> V2 Int -> Maybe a
guiClick w p = para alg w
  where
    firstJust [] = Nothing
    firstJust (Just x:_) = Just x
    firstJust (_:xs) = firstJust xs

    alg TextF{} = Nothing
    alg EmptyF = Nothing
    --alg ImageF{} = Nothing
    alg (BoxF _ r rs) = firstJust $ snd <$> (r:rs)
    alg (ButtonF (Size sz) a (r,a'))
      | p `insideIncl` (mkAABox 0 sz <> guiSize r) = Just a
      | otherwise = a'
    alg (PanelF _ ra) = snd ra


-- rendering thought:
-- http://www.iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
-- combine (soft-min?) a bunch of primitives with rounding on layers
-- layers define opaque colour, outline, glow, drop shadow based on unified SDF
-- intersperse text or icons from an atlas

-- c++ references
-- https://philippegroarke.com/posts/2018/c++_ui_solutions/
