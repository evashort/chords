module SharpCount exposing (fromFlavor, fromTonic)

fromFlavor : Int -> List Int -> List Int
fromFlavor rootSharpCount flavor =
  List.scanl
    (+)
    rootSharpCount
    (List.map delta (intervals flavor))

-- Tritones are considered diminished 5ths
delta : Int -> Int
delta interval =
  (7 * interval + 6) % 12 - 6

intervals : List Int -> List Int
intervals flavor =
  List.map2 (-) flavor (0 :: flavor)

fromTonic : Int -> Int
fromTonic tonic =
  case tonic of
    7 -> 1
    2 -> 2
    9 -> 3
    4 -> 4
    11 -> 5
    _ -> 0
