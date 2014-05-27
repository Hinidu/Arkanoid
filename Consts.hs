module Consts where

import Graphics.UI.SDL as SDL

import Geometry.Types
import Graphics.Colors
import Util

width, height :: Int
width = 480
height = 480

ballRadius :: Float
ballRadius = 0.03

ballSpeed :: Float
ballSpeed = 0.007

ballColor :: SDL.Color
ballColor = white

paddleWidth, paddleHeight :: Float
paddleWidth = 0.1
paddleHeight = 0.03

paddleSize :: Size
paddleSize = (paddleWidth, paddleHeight)

paddleSpeed :: Float
paddleSpeed = 0.008

paddleColor :: SDL.Color
paddleColor = red

allBricksHeight :: Float
allBricksHeight = 0.5

brickRows, brickCols :: Int
brickRows = 6
brickCols = 12

brickSize :: Size
brickSize =
    zipPairWith (/) (1.0, allBricksHeight)
        $ mapPair fromIntegral (brickCols, brickRows)
