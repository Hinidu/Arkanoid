{-# LANGUAGE NamedFieldPuns  #-}
module Hage.Game.Arkanoid.EventHandler where

import Control.Arrow
import qualified Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.SDL

import Hage.Game.Arkanoid.Consts
import Hage.Game.Arkanoid.GameState

keyboardHandle :: WrappedEvent t -> Event t (GameState -> GameState)
keyboardHandle esdl =
    (keyDownHandle <$> keyDownEvent esdl)
    `union` (keyUpHandle <$> keyUpEvent esdl)

keyDownHandle :: SDL.Keysym -> GameState -> GameState
keyDownHandle ks gs@GameState{ball} =
    case SDL.symKey ks of
        SDL.SDLK_LEFT  -> changePaddleSpeed gs (-paddleSpeed)
        SDL.SDLK_RIGHT -> changePaddleSpeed gs paddleSpeed
        SDL.SDLK_SPACE -> gs { ball = second (const (ballSpeed, -ballSpeed)) ball }
        _              -> gs

keyUpHandle :: SDL.Keysym -> GameState -> GameState
keyUpHandle ks gs =
    case SDL.symKey ks of
        SDL.SDLK_LEFT  -> changePaddleSpeed gs paddleSpeed
        SDL.SDLK_RIGHT -> changePaddleSpeed gs (-paddleSpeed)
        _              -> gs
