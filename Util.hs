module Util where

import Control.Arrow
import Control.Monad

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair = join (***)

zipPairWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipPairWith f (x, y) (v, u) = (x `f` v, y `f` u)
