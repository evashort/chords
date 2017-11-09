module Chord exposing (Chord, get, invert, intervals, indexOf)

type alias Chord = List Int

get : Chord -> Int -> Int
get chord n =
  let
    i = n % List.length chord
  in let
    octave = (n - i) // List.length chord
  in
    case List.drop i chord of
      pitch :: _ -> 12 * octave + pitch
      [] -> 0

invert : Int -> Chord -> Chord
invert n chord =
  List.map (get chord) (List.range n (n + List.length chord - 1))

intervals : Chord -> List Int
intervals chord =
  List.map2 (-) (invert 1 chord) chord

indexOf : Int -> Chord -> List Int
indexOf pitch =
  zeros << List.map ((+) -pitch)

zeros : Chord -> List Int
zeros chord =
  List.filterMap
    identity
    (List.indexedMap (zerosHelp (List.length chord)) chord)

zerosHelp : Int -> Int -> Int -> Maybe Int
zerosHelp n i pitch =
  if pitch % 12 == 0 then
    Just (i - n * (pitch // 12))
  else
    Nothing
