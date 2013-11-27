module Graphics where

import System.Random
import Graphics.UI.GLUT hiding (normalize)

import Consts
import Geometry

normalize :: GLfloat -> GLfloat
normalize = (subtract $ paneSize / 2) . (* paneSize)

top = normalize 1
middle = normalize 0.5
bottom = normalize 0
left = bottom
right = top

topSide = ((left, top), (paneSize, 1.0))
bottomSide = ((left, bottom - 1.0), (paneSize, 1.0))
leftSide = ((left - 1.0, bottom), (1.0, paneSize))
rightSide = ((right, bottom), (1.0, paneSize))
windowSides = [topSide, bottomSide, leftSide, rightSide]

inWindow :: Point -> Bool
inWindow (x, y) = left <= x && x <= right && bottom <= y && y <= top

instance (Random a, Num a) => Random (Color4 a) where
    randomR (Color4 r1 g1 b1 a1, Color4 r2 g2 b2 a2) gen0 =
        let (r, gen1) = randomR (r1, r2) gen0
            (g, gen2) = randomR (g1, g2) gen1
            (b, gen3) = randomR (b1, b2) gen2
            (a, gen4) = randomR (a1, a2) gen3
        in  (Color4 r g b a, gen4)
    random gen = randomR (Color4 0 0 0 0, Color4 1 1 1 1) gen
