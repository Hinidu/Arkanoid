module EventNetwork where

import System.Random

import Graphics.UI.SDL as SDL
import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics
import Reactive.Banana.Frameworks

import Display
import GameState as GS
import Graphics

setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd = do
    rndGen <- liftIO getStdGen
    eTickDiff <- tickDiffEvent es
    esdl <- sdlEvent es
    let
        -- Initial GameState
        gsInitial = GS.initGame rndGen

        -- We always use the same screen
        bScreen = pure $ gdScreen gd

        -- GameState update event
        eGSChange = (GS.update <$> eTickDiff) --`union` (updateGSOnKey <$> keyDownEvent esdl)

        -- GameState behavior
        bGameState = accumB gsInitial eGSChange
    liftIO $ print $ paddle gsInitial
    renderGraph (display <$> bGameState) bScreen
    return ()
