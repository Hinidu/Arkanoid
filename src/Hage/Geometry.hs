module Hage.Geometry where

import qualified Graphics.UI.SDL as SDL (Rect(..))

import Hage.Geometry.Types

infinity :: Float
infinity = 1.0e9

movePoint :: Vector -> Point -> Point
movePoint (vx, vy) (px, py) = (px + vx, py + vy)

nullVector :: Vector
nullVector = (0, 0)

mirrorX :: Vector -> Vector
mirrorX (vx, vy) = (-vx, vy)

mirrorY :: Vector -> Vector
mirrorY (vx, vy) = (vx, -vy)

isBetween :: Ord a => a -> a -> a -> Bool
isBetween a b x = a <= x && x <= b

makeRect :: Point -> Size -> SDL.Rect
makeRect (x, y) (w, h) = SDL.Rect (truncate x) (truncate y) (truncate w) (truncate h)

hasRectPoint :: Rect -> Point -> Bool
hasRectPoint ((rx, ry), (w, h)) (x, y) =
    isBetween rx (rx + w) x && isBetween ry (ry + h) y

circleAndRectClashing :: Circle -> Rect -> Maybe Clash
circleAndRectClashing ((cx, cy), r) rect
    | check [(cx + r, cy), (cx - r, cy)] = Just Horizontal
    | check [(cx, cy + r), (cx, cy - r)] = Just Vertical
    | otherwise                          = Nothing
    where
        check = any $ hasRectPoint rect
