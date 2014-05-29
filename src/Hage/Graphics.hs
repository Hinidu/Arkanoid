module Hage.Graphics where

import System.Random
import Graphics.UI.SDL as SDL
import Reactive.Banana.SDL.Graphics

import Hage.Game.Arkanoid.Consts
import Hage.Geometry.Types
import Hage.Util

newtype GraphicsData = GraphicsData { gdScreen :: Screen }

type Position = (Int, Int)

initGraphics :: IO GraphicsData
initGraphics = do
    SDL.init [SDL.InitEverything]
    SDL.enableUnicode True
    SDL.setVideoMode width height 32 []
    SDL.setCaption "Arkanoid: the Haskell version" "Arkanoid"
    mainSurf <- SDL.getVideoSurface
    return $ GraphicsData mainSurf

worldToScreen :: Point -> Position
worldToScreen (x, y) =
    mapPair truncate
    $ zipPairWith (*) (x, y)
    $ mapPair fromIntegral (width, height)

screenToWorld :: Position -> Point
screenToWorld (x, y) =
    (fromIntegral x / fromIntegral width , fromIntegral y / fromIntegral height)

top, right, middle, bottom, left :: Float
top = 0.0
right = 1.0
middle = 0.5
bottom = 1.0
left = 0.0

topSide = ((left, top - 1.0), (right - left, 1.0))
bottomSide = ((left, bottom), (right - left, 1.0))
leftSide = ((left - 1.0, top), (1.0, bottom - top))
rightSide = ((right, top), (1.0, bottom - top))
windowSides = [topSide, bottomSide, leftSide, rightSide]

inWindow :: Point -> Bool
inWindow (x, y) = left <= x && x <= right && bottom <= y && y <= top

instance Random SDL.Color where
    randomR (SDL.Color r1 g1 b1, SDL.Color r2 g2 b2) gen0 =
        let (r, gen1) = randomR (r1, r2) gen0
            (g, gen2) = randomR (g1, g2) gen1
            (b, gen3) = randomR (b1, b2) gen2
        in  (SDL.Color r g b, gen3)
    random = randomR (SDL.Color 0 0 0, SDL.Color 255 255 255)
