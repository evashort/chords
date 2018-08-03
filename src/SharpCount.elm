module SharpCount exposing (fromFlavor)

fromFlavor : Int -> List Int -> List Int
fromFlavor rootSharpCount flavor =
  List.scanl
    (+)
    rootSharpCount
    (List.map delta (intervals flavor))

delta : Int -> Int
delta interval =
  if interval == 6 then
    Debug.crash "SharpCount.delta: Tritones are ambiguous"
  else
    (7 * interval + 6) % 12 - 6

intervals : List Int -> List Int
intervals flavor =
  List.map2 (-) flavor (0 :: flavor)
