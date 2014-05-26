module Main where

import qualified Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.Frameworks (actuate)
import Reactive.Banana.SDL

import EventNetwork
import Graphics

main :: IO ()
main = do
    sdlES <- getSDLEventSource
    gd <- initGraphics
    network <- compile $ setupNetwork sdlES gd
    actuate network
    runCappedSDLPump 60 sdlES
    SDL.quit
