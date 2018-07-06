module StrumPattern exposing (StrumPattern(..), StrumNote, notes)

import Chord exposing (Chord)

type StrumPattern
  = Basic
  | Indie
  | Modern

type alias StrumNote =
  { t : Float
  , strumIndex : Int
  , v : Float
  , f : Float
  }

notes : StrumPattern -> Bool -> Int -> Chord -> List StrumNote
notes pattern highStart lowestNote chord =
  let
    rootPitch =
      (chord.root - lowestNote) % 12 + lowestNote
  in let
    chordFrequencies =
      List.map
        (pitchFrequency << (+) rootPitch)
        (0 :: chord.flavor)
  in let
    stringFrequencies =
      List.map
        (stringFrequency chordFrequencies)
        triad
  in let
    patternStrums =
      case pattern of
        Basic -> basic
        Indie -> indie
        Modern -> modern
  in let
    strums =
      if highStart then
        List.drop 8 patternStrums ++ patternStrums
      else
        patternStrums ++ patternStrums
  in
    List.concat
      ( List.indexedMap
          (strumNotes stringFrequencies)
          strums
      )

pitchFrequency : Int -> Float
pitchFrequency pitch =
  440 * 2 ^ (toFloat (pitch - 69) / 12)

stringFrequency : List Float -> Int -> Float
stringFrequency frequencies string =
  let
    lowFrequency =
      case List.drop (abs (rem string 10)) frequencies of
        f :: _ ->
          f
        [] ->
          Debug.crash
            ("StrumPattern.stringFrequency: Bad string: " ++ toString string)
  in let
    octave = string // 10
  in
    lowFrequency * toFloat (2 ^ octave)

strumNotes : List Float -> Int -> Maybe Strum -> List StrumNote
strumNotes strings i maybeStrum =
  case maybeStrum of
    Nothing ->
      []
    Just strum ->
      let
        v = 0.01 * toFloat strum.v
      in let
        droppedStrings = floor strum.missedStrings
      in let
        missFraction =
          strum.missedStrings - toFloat droppedStrings
      in let
        vs =
          if missFraction > 0 then
            (++)
              ( List.repeat
                  (List.length strings - droppedStrings - 1)
                  v
              )
              [ (1 - missFraction) * v ]
          else
            List.repeat
              (List.length strings - droppedStrings)
              v
      in let
        t = 0.25 * toFloat i
      in let
        orderedStrings =
          if strum.up then
            List.reverse strings
          else
            strings
      in
        indexedMap2 (StrumNote t) vs orderedStrings

indexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
indexedMap2 f xs ys =
  List.map3
    f
    ( List.range
        0
        (max (List.length xs) (List.length ys) - 1)
    )
    xs
    ys

triad : List Int
triad = [ -10, -12, 0, 1, 2, 10 ]

type alias Strum =
  { up : Bool
  , v : Int
  , missedStrings : Float
  }

up : Int -> Float -> Maybe Strum
up v missedStrings =
  Just (Strum True v missedStrings)

down : Int -> Float -> Maybe Strum
down v missedStrings =
  Just (Strum False v missedStrings)

basic : List (Maybe Strum)
basic =
  [ down 92 0,    Nothing, Nothing,    Nothing
  , down 81 0.09, Nothing, up 92 0.13, Nothing
  , Nothing,      Nothing, up 70 1.06, Nothing
  , down 81 0.09, Nothing, up 78 1,    Nothing
  ]

indie : List (Maybe Strum)
indie =
  [ down 85 0,  Nothing, down 62 0, Nothing
  , down 108 0, Nothing, down 67 0, up 77 1.38
  , down 85 0,  Nothing, down 62 0, Nothing
  , down 108 0, Nothing, down 67 0, up 77 1.38
  ]

modern : List (Maybe Strum)
modern =
  [ down 99 0.07, Nothing,    Nothing,   Nothing
  , down 92 0,    Nothing,    Nothing,   up 77 0.29
  , down 66 0.21, up 81 1.32, down 81 0, Nothing
  , down 99 0,    Nothing,    down 81 0, up 81 1.32
  ]
