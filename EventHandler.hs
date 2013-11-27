module EventHandler where

import Control.Monad (unless)
import Data.IORef
import Graphics.UI.GLUT

import Consts
import GameState

mkKeyboardHandle :: IO (IORef GameState -> KeyboardMouseCallback)
mkKeyboardHandle = fmap keyboardHandle $ newIORef []

keyboardHandle :: IORef [Key] -> IORef GameState -> KeyboardMouseCallback
keyboardHandle pressedKeysRef gameRef key updown _ _ = do
    pressedKeys <- get pressedKeysRef
    game <- get gameRef
    let changePaddleSpeed ds = let (p, (s, _)) = paddle game
                                   s' = if s == ds then s else s + ds
                               in gameRef $= game { paddle = (p, (s', 0)) }
    let runBall = let (p, _) = ball game
                  in gameRef $= game { ball = (p, (ballSpeed, ballSpeed)) }
    case updown of
        Up   -> do
                pressedKeysRef $= [k | k <- pressedKeys, k /= key]
                case key of
                    SpecialKey KeyLeft  -> changePaddleSpeed paddleSpeed
                    SpecialKey KeyRight -> changePaddleSpeed (-paddleSpeed)
                    _                   -> return ()
        Down -> unless (key `elem` pressedKeys) $ do
                pressedKeysRef $= key:pressedKeys
                case key of
                    SpecialKey KeyLeft  -> changePaddleSpeed (-paddleSpeed)
                    SpecialKey KeyRight -> changePaddleSpeed paddleSpeed
                    Char ' '            -> runBall
                    _                   -> return ()
