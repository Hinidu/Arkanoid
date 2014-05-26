module Main where

import System.Random
import Data.IORef
import Graphics.UI.GLUT
import qualified Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.Frameworks (actuate)
import Reactive.Banana.SDL

import Display
import EventHandler
import EventNetwork
import GameState
import Graphics

main :: IO ()
main = do
    sdlES <- getSDLEventSource
    gd <- initGraphics
    network <- compile $ setupNetwork sdlES gd
    actuate network
    runSDLPump sdlES
    SDL.quit

    -- (progName, _) <- getArgsAndInitialize
    -- initialDisplayMode $= [DoubleBuffered]
    -- createWindow progName
    -- -- fullScreen
    -- gameRef <- fmap initGame getStdGen >>= newIORef 
    -- displayCallback $= display gameRef
    -- idleCallback $= Just (update gameRef)
    -- keyboardHandle <- mkKeyboardHandle
    -- keyboardMouseCallback $= Just (keyboardHandle gameRef)
    -- mainLoop
