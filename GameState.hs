module GameState where

import System.Random (StdGen(..), randoms)
import Data.IORef (IORef(..))
import Data.List (partition)
import Data.Maybe (catMaybes)
import Graphics.UI.GLUT (Color4(..), GLfloat(..), ($=), get, postRedisplay)

import Consts
import Geometry
import Graphics

type Brick = (Point, Color4 GLfloat)

type MovingObject = (Point, Vector)

data GameState = GameState { paddle, ball :: MovingObject, bricks :: [Brick] }
    deriving (Show)

initGame :: StdGen -> GameState
initGame rndGen = GameState
    { paddle = ((middle - paddleWidth / 2, bottom), nullVector)
    , ball = ((middle, bottom + paddleHeight + ballRadius), nullVector)
    , bricks = zip points colors
    }
    where
        getx j = fromIntegral j * brickWidth - paneSize / 2
        gety i = negate $ fromIntegral i * brickHeight - paneSize / 2
        xs = map getx [0..brickCols-1]
        ys = map gety [0..brickRows-1]
        points = [(x, y) | x <- xs , y <- ys]
        colors = take (brickRows * brickCols) $ randoms rndGen

update :: IORef GameState -> IO ()
update gameRef = do
    game <- get gameRef
    let (ball', bricks') = moveBall game
    gameRef $= game { ball = ball', paddle = movePaddle game, bricks = bricks' }
    postRedisplay Nothing

moveBall :: GameState -> (MovingObject, [Brick])
moveBall game =
    let (p, v@(dx, dy)) = ball game
        p'@(x, y) = movePoint v p

        brickSize = (brickWidth, brickHeight)
        (hit, notHit) = partition (ballClashRect p' brickSize . fst) $ bricks game

        paddleSize = (paddleWidth, paddleHeight)
        canReflectFrom = (fst $ paddle game, paddleSize):windowSides
            ++ (map (\brick -> (fst brick, brickSize)) hit)
        clashes = catMaybes $ map (circleAndRectClashing (p', ballRadius)) canReflectFrom

        dx' = if Horizontal `elem` clashes then -dx else dx
        dy' = if Vertical `elem` clashes then -dy else dy
    in  ((p', (dx', dy')), notHit)

ballClashRect ballPos size rectPos =
    circleAndRectClashing (ballPos, ballRadius) (rectPos, size) /= Nothing

movePaddle :: GameState -> MovingObject
movePaddle game =
    let (p, v) = paddle game
        (x, y) = movePoint v p
        x' = max left $ min x $ right - paddleWidth
        y' = max bottom $ min top y
    in  ((x', y'), v)
