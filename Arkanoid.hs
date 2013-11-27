module Main where

import System.Random
import Data.IORef
import Graphics.UI.GLUT

import Display
import EventHandler
import GameState

main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow progName
    -- fullScreen
    gameRef <- fmap initGame getStdGen >>= newIORef 
    displayCallback $= display gameRef
    idleCallback $= Just (update gameRef)
    keyboardHandle <- mkKeyboardHandle
    keyboardMouseCallback $= Just (keyboardHandle gameRef)
    mainLoop
