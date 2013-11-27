module Display where

import Data.IORef
import Graphics.UI.GLUT

import Consts
import Geometry
import GameState

display :: IORef GameState -> DisplayCallback
display gameRef = do
    clear [ColorBuffer]
    game <- get gameRef
    displayRect (paddleWidth, paddleHeight) (fst $ paddle game) paddleColor
    displayCircle ballRadius (fst $ ball game) ballColor
    mapM_ (uncurry $ displayRect (brickWidth, brickHeight)) $ bricks game
    swapBuffers

displayRect :: Point -> Point -> Color4 GLfloat -> IO ()
displayRect (width, height) (x, y) col = do
    currentColor $= col
    renderPrimitive Quads $ mapM_ makeVertex
        [(x, y), (x + width, y), (x + width, y + height), (x, y + height)]

displayCircle :: GLfloat -> Point -> Color4 GLfloat -> IO ()
displayCircle r (x0, y0) col = do
    currentColor $= col
    renderPrimitive Polygon $
        mapM_ (\(x, y) -> makeVertex (x0 + r * x, y0 + r * y)) circlePoints

circlePoints :: [Point]
circlePoints =
    let pointsCount = 32.0
    in  [(x, y)
        | i <- [1.0..pointsCount]
        , let angle = i / pointsCount * 2 * pi
        , let x = cos angle
        , let y = sin angle
        ]

makeVertex :: (GLfloat, GLfloat) -> IO ()
makeVertex (x, y) = vertex $ Vertex3 x y 0
