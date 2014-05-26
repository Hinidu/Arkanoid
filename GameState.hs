module GameState where

import System.Random (StdGen, randoms)
import Data.List (partition)
import Data.Maybe (isJust, mapMaybe)
import Data.Word (Word32)
import Graphics.UI.SDL as SDL (Color(..))

import Consts
import Geometry
import Geometry.Types
import Graphics

type Brick = (Point, SDL.Color)

type MovingObject = (Point, Vector)

data GameState = GameState { paddle, ball :: MovingObject, bricks :: [Brick] }
    deriving (Show)

initGame :: StdGen -> GameState
initGame rndGen = GameState
    { paddle = ((middle - paddleWidth / 2, bottom - paddleHeight), nullVector)
    , ball = ((middle, bottom - paddleHeight - ballRadius), nullVector)
    , bricks = zip points colors
    }
    where
        getx j = fromIntegral j * getWidth brickSize
        gety i = fromIntegral i * getHeight brickSize
        xs = map getx [0..brickCols-1]
        ys = map gety [0..brickRows-1]
        points = [(x, y) | x <- xs , y <- ys]
        colors = take (brickRows * brickCols) $ randoms rndGen

update :: Word32 -> GameState -> GameState
update _ game =
    let (ball', bricks') = moveBall game
    in game { ball = ball', paddle = movePaddle game, bricks = bricks' }

moveBall :: GameState -> (MovingObject, [Brick])
moveBall game =
    let (p, v@(dx, dy)) = ball game
        p' = movePoint v p

        (hit, notHit) = partition (ballClashRect p' brickSize . fst) $ bricks game

        canReflectFrom = [(fst $ paddle game, paddleSize), topSide, leftSide, rightSide]
            ++ map (flip (,) brickSize . fst) hit
        clashes = mapMaybe (circleAndRectClashing (p', ballRadius)) canReflectFrom

        dx' = if Horizontal `elem` clashes then -dx else dx
        dy' = if Vertical `elem` clashes then -dy else dy
    in  ((p', (dx', dy')), notHit)

ballClashRect :: Point -> Size -> Point -> Bool
ballClashRect ballPos size rectPos =
    isJust $ circleAndRectClashing (ballPos, ballRadius) (rectPos, size)

movePaddle :: GameState -> MovingObject
movePaddle game =
    -- let (p, v) = paddle game
    --     (x, y) = movePoint v p
    --     x' = max left $ min x $ right - paddleWidth
    --     y' = max bottom $ min top y
    -- in  ((x', y'), v)
    paddle game
