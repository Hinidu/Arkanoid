{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
module Hage.Game.Arkanoid.Display where

import Control.Arrow
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import Reactive.Banana.SDL.Graphics

import Hage.Game.Arkanoid.Consts
import Hage.Game.Arkanoid.GameState
import Hage.Geometry.Types
import Hage.Graphics
import Hage.Graphics.Colors
import Hage.Util

display :: GameState -> Graphic
display game =
    let paddleRect = (fst $ paddle game, paddleSize)
        toRect pos = (pos, brickSize)
        bricks' = map (first toRect) $ bricks game
    in  displayRect paddleRect paddleColor
        `over` draw (fst $ ball game, ballRadius) ballColor
        `over` foldl (\g brick -> uncurry displayRect brick `over` g) emptyG bricks'
        `over` emptyScreen

emptyScreen :: Graphic
emptyScreen = displayRect ((0, 0), (1.0, 1.0)) black

displayRect :: Rect -> SDL.Color -> Graphic
displayRect rect col =
    let ((x, y), (w, h)) = mapPair worldToScreen rect
    in  draw (Fill (Just $ SDL.Rect x y w h) col) (Mask Nothing 0 0)

instance Draw Circle SDL.Color where
    draw (p, r) color = Graphic $ \scr ->
        let (x, y) = worldToScreen p
            pixel = (SDL.mapRGB . SDL.surfaceGetPixelFormat) scr (SDL.colorRed color) (SDL.colorGreen color) (SDL.colorBlue color)
            r' = truncate $ fromIntegral height * r
        in  pixel >>= \c -> SDL.filledCircle scr (fromIntegral x) (fromIntegral y) r' c >> return Nothing
