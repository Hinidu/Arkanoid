module Display where

import Control.Arrow
import qualified Graphics.UI.SDL as SDL
import Reactive.Banana.SDL.Graphics

import Consts
import Geometry.Types
import Graphics
import Graphics.Colors
import GameState
import Util

display :: GameState -> Graphic
display game =
    let paddleRect = (fst $ paddle game, paddleSize)
        ballRect = (zipPairWith (-) (fst $ ball game) (ballRadius, ballRadius), (2 * ballRadius, 2 * ballRadius))
        toRect pos = (pos, brickSize)
        bricks' = map (first toRect) $ bricks game
    in  displayRect paddleRect paddleColor
        `over` displayRect ballRect ballColor
        `over` foldl (\g brick -> uncurry displayRect brick `over` g) emptyG bricks'
        `over` emptyScreen

emptyScreen :: Graphic
emptyScreen = displayRect ((0, 0), (1.0, 1.0)) black

displayRect :: Rect -> SDL.Color -> Graphic
displayRect rect col =
    let ((x, y), (w, h)) = mapPair worldToScreen rect
    in  draw (Fill (Just $ SDL.Rect x y w h) col) (Mask Nothing 0 0)
