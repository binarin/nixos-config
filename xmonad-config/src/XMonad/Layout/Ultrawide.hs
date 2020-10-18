{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Layout.Ultrawide (Ultrawide(..)) where

import Control.Monad (msum)

import XMonad
import XMonad.StackSet as W
import XMonad.Util.XUtils (fi)

data Ultrawide a = Ultrawide
  { uwNMaster :: !Int
  , uwDelta :: !Rational
  , uwFrac :: !Rational
  } deriving (Show, Read)

instance LayoutClass Ultrawide a where
  pureLayout (Ultrawide nm dt frac) rt stack = zipWith (,) ws rects
    where
      ws = W.integrate stack
      rects = uwTiles frac rt nm (length ws)

  pureMessage uw@(Ultrawide nm dt frac) m = msum [ resize <$> fromMessage m
                                                 , changeMaster <$> fromMessage m
                                                 ]
    where
      resize Shrink = uw { uwFrac = max (1/4) (frac - dt) }
      resize Expand = uw { uwFrac = min 1 (frac + dt) }
      changeMaster (IncMasterN x) = uw { uwNMaster = max 1 (min 3 (nm + x)) }

  description _ = "UW"

uwTiles :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
uwTiles frac rect nMaster n
  | n <= nMaster = masterRects
  | n <= nMaster + 1 = masterRects ++ rightRects
  | otherwise = masterRects ++ leftRects ++ rightRects
  where
    (leftRect, masterRect, rightRect) = split3 frac rect
    masterRects = splitVertically nMaster masterRect
    nSlaves = n - nMaster
    nLeftSlaves = nSlaves `div` 2
    nRightSlaves = n - nMaster - nLeftSlaves
    leftRects = splitVertically nLeftSlaves leftRect
    rightRects = splitVertically nRightSlaves rightRect

split3 :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3 f (Rectangle x y w h) =
  ( Rectangle x y leftSlave h
  , Rectangle (x + fi leftSlave) y masterWidth h
  , Rectangle (x + fi leftSlave + fi masterWidth) y rightSlave h
  )
  where
    masterWidth = ceiling $ fi w * f
    leftSlave = ceiling $ (fi w - fi masterWidth) / 2
    rightSlave = w - masterWidth - leftSlave
