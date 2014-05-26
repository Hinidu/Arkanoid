module EventNetwork where

import System.Random

import Reactive.Banana
import Reactive.Banana.SDL
import Reactive.Banana.SDL.Graphics
import Reactive.Banana.Frameworks

import Display
import EventHandler
import GameState as GS
import Graphics

setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd = do
    rndGen <- liftIO getStdGen
    eTickDiff <- tickDiffEvent es
    esdl <- sdlEvent es
    let gsInitial = GS.initGame rndGen
        bScreen = pure $ gdScreen gd
        eGSChange = (GS.update <$> eTickDiff) `union` keyboardHandle esdl
        bGameState = accumB gsInitial eGSChange
    renderGraph (display <$> bGameState) bScreen
