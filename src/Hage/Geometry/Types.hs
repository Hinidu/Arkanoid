module Hage.Geometry.Types where

type Point = (Float, Float)

type Vector = (Float, Float)

type Circle = (Point, Float)

type Size = (Float, Float)

type Rect = (Point, Size)

data Clash = Horizontal | Vertical
    deriving (Eq)

getWidth :: Size -> Float
getWidth = fst

getHeight :: Size -> Float
getHeight = snd
