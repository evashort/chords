module StrumPattern exposing (StrumPattern(..), defaultBpm, StrumNote, notes)

import Chord exposing (Chord)

import Dict exposing (Dict)

type StrumPattern
  = Basic
  | Indie
  | Modern

defaultBpm : StrumPattern -> Float
defaultBpm strumPattern =
  case strumPattern of
    Basic ->
      125
    Indie ->
      85
    Modern ->
      95

type alias StrumNote =
  { t : Float
  , strumIndex : Int
  , v : Float
  , f : Float
  }

notes : StrumPattern -> Bool -> Int -> Chord -> List StrumNote
notes pattern highStart lowestPitch chord =
  let
    rootPitch =
      (chord.root - lowestPitch) % 12 + lowestPitch
  in let
    chordFrequencies =
      List.map
        (pitchFrequency << (+) rootPitch)
        (0 :: chord.flavor)
  in let
    strings =
      case Dict.get chord.flavor schemes of
        Just scheme ->
          if rootPitch < 48 then
            scheme.low
          else if rootPitch < 52 then
            scheme.mid
          else
            scheme.high
        Nothing ->
          List.range 0 (List.length chord.flavor)
  in let
    stringFrequencies =
      List.map
        (stringFrequency chordFrequencies)
        strings
  in let
    patternStrums =
      case pattern of
        Basic ->
          basic
        Indie ->
          indie
        Modern ->
          modern
  in let
    strums =
      case pattern of
        Basic ->
          patternStrums ++ patternStrums
        Indie ->
          patternStrums ++ patternStrums
        Modern ->
          if highStart then
            List.drop 8 patternStrums ++ patternStrums
          else
            patternStrums
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

type alias Scheme =
  { low : List Int -- B2 and below
  , mid : List Int -- C3 to Eb3
  , high : List Int -- E3 and above
  }

schemes : Dict (List Int) Scheme
schemes =
  Dict.fromList
    [ ( [ 4, 7 ], triad )
    , ( [ 3, 7 ], triad )
    , ( [ 3, 6 ], triad )
    , ( [ 4, 8 ], triad )
    , ( [ 5, 7 ], triad )
    , ( [ 2, 7 ], triad )
    , ( [ 4, 7, 10 ], seventh )
    , ( [ 4, 7, 11 ], seventh )
    , ( [ 3, 7, 10 ], seventh )
    , ( [ 4, 7, 9 ], seventh )
    , ( [ 3, 6, 10 ], seventh )
    , ( [ 3, 7, 9 ], seventh )
    , ( [ 3, 6, 9 ], seventh )
    , ( [ 3, 7, 11 ], seventh )
    , ( [ 4, 7, 10, 14 ], ninth )
    , ( [ 4, 7, 11, 14 ], ninth )
    , ( [ 4, 7, 14 ], addNinth )
    , ( [ 3, 7, 14 ], addNinth )
    , ( [ 3, 7, 10, 14 ], ninth )
    , ( [ 4, 7, 10, 13 ], ninth )
    , ( [ 4, 7, 10, 14, 21 ], thirteenth )
    , ( [ 4, 7, 11, 14, 21 ], thirteenth )
    , ( [ 3, 7, 10, 14, 21 ], thirteenth )
    ]

triad : Scheme
triad =
  { low = [ 0, 2, 10, 11, 12 ]
  , mid = [ 0, 1, 2, 10, 11 ]
  , high = [ -10, -12, 0, 1, 2, 10 ]
  }

seventh : Scheme
seventh =
  { low = [ 0, 2, 10, 11, 13 ]
  , mid = [ 0, 2, 3, 11 ]
  , high = [ -10, -12, 0, 1, 3, 10 ]
  }

ninth : Scheme
ninth =
  { low = [ 0, 1, 3, 4 ]
  , mid = [ 0, 1, 3, 4 ]
  , high = [ -10, -12, 1, 3, 4 ]
  }

addNinth : Scheme
addNinth =
  { low = [ 0, 1, 2, 3 ]
  , mid = [ 0, 1, 2, 3 ]
  , high = [ -10, -12, 1, 2, 3 ]
  }

thirteenth : Scheme
thirteenth =
  { low = [ 0, 1, 3, 4, 5 ]
  , mid = [ 0, 1, 3, 4, 5 ]
  , high = [ -10, -12, 1, 3, 4, 5 ]
  }

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
  ]

modern : List (Maybe Strum)
modern =
  [ down 99 0.07, Nothing,    Nothing,   Nothing
  , down 92 0,    Nothing,    Nothing,   up 77 0.29
  , down 66 0.21, up 81 1.32, down 81 0, Nothing
  , down 99 0,    Nothing,    down 81 0, up 81 1.32
  ]
