module Main where

import qualified Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.Frameworks (actuate)
import Reactive.Banana.SDL

import Hage.Graphics
import Hage.Game.Arkanoid.EventNetwork

main :: IO ()
main = do
    sdlES <- getSDLEventSource
    gd <- initGraphics
    network <- compile $ setupNetwork sdlES gd
    actuate network
    runCappedSDLPump 60 sdlES
    SDL.quit
