module Util where
import List as L
import Model exposing (..)
import Dict as D

locToVect (x,y) = [x,y]

vectToLoc v = case v of
                (x::y::_) -> (x,y)
                _ -> (0, 0)

radius = 5

findPointAtLocation : Location -> D.Dict Int Point -> Maybe Int
findPointAtLocation l ps =
  let go xs = case xs of
    [] -> Nothing
    (i,p)::rest -> if sqDist l p.location <= radius^2
                   then Just i
                   else go rest
  in go (D.toList ps)

sqDist (x,y) (x',y') = (x-x')^2 + (y-y')^2
