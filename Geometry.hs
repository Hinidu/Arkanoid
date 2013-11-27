module Geometry where

import Graphics.UI.GLUT (GLfloat(..))

import Consts

type Point = (GLfloat, GLfloat)

type Vector = (GLfloat, GLfloat)

type Circle = (Point, GLfloat)

type Size = (GLfloat, GLfloat)

type Rect = (Point, Size)

data Clash = Horizontal | Vertical
    deriving (Eq)

infinity :: GLfloat
infinity = 1.0e9

movePoint :: Vector -> Point -> Point
movePoint (vx, vy) (px, py) = (px + vx, py + vy)

nullVector :: Vector
nullVector = (0, 0)

mirrorX :: Vector -> Vector
mirrorX (vx, vy) = (-vx, vy)

mirrorY :: Vector -> Vector
mirrorY (vx, vy) = (vx, -vy)

isBetween a b x = a <= x && x <= b

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
