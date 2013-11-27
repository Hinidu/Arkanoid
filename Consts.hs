module Consts where

import Graphics.UI.GLUT

paneSize :: GLfloat
paneSize = 2.0

ballRadius = 0.03 * paneSize

ballSpeed = 0.005 * paneSize

ballColor :: Color4 GLfloat
ballColor = Color4 1 1 1 1

paddleHeight = 0.03 * paneSize
paddleWidth = 0.1 * paneSize

paddleSpeed = 0.005 * paneSize

paddleColor :: Color4 GLfloat
paddleColor = Color4 1 0 0 1

allBricksHeight = 0.5 * paneSize

brickRows :: Int
brickRows = 6
brickCols :: Int
brickCols = 12

brickHeight = allBricksHeight / fromIntegral brickRows
brickWidth = paneSize / fromIntegral brickCols
