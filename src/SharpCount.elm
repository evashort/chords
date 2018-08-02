module SharpCount exposing (ranges)

import Chord

import Dict exposing (Dict)

range : List Int -> (Int, Int)
range flavor =
  let
    sharpCounts =
      List.scanl
        (+)
        0
        (List.map sharpDelta (intervals flavor))
  in let
    minimum =
      Maybe.withDefault 1000 (List.minimum sharpCounts)
    maximum =
      Maybe.withDefault 1000 (List.maximum sharpCounts)
  in
    ( -minimum, 5 - maximum )

sharpDelta : Int -> Int
sharpDelta interval =
  if interval == 6 then
    Debug.crash "think about this"
  else
    (7 * interval + 6) % 12 - 6

intervals : List Int -> List Int
intervals flavor =
  List.map2 (-) flavor (0 :: flavor)

ranges : Dict String (Int, Int)
ranges =
  Dict.map
    (always range)
    Chord.flavors
