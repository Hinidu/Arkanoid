module EventHandler where

import qualified Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.SDL

import Consts
import GameState

keyboardHandle :: WrappedEvent t -> Event t (GameState -> GameState)
keyboardHandle esdl =
    (keyDownHandle <$> keyDownEvent esdl)
    `union` (keyUpHandle <$> keyUpEvent esdl)

keyDownHandle :: SDL.Keysym -> GameState -> GameState
keyDownHandle ks gs =
    case SDL.symKey ks of
        SDL.SDLK_LEFT  -> changePaddleSpeed gs (-paddleSpeed)
        SDL.SDLK_RIGHT -> changePaddleSpeed gs paddleSpeed
        _              -> gs

keyUpHandle :: SDL.Keysym -> GameState -> GameState
keyUpHandle ks gs =
    case SDL.symKey ks of
        SDL.SDLK_LEFT  -> changePaddleSpeed gs paddleSpeed
        SDL.SDLK_RIGHT -> changePaddleSpeed gs (-paddleSpeed)
        _              -> gs
