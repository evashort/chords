module Arp exposing (pad, strum, intro, continuation)

import Chord exposing (Chord)
import Note exposing (Note)

import Dict exposing (Dict)

pad : Int -> Chord -> List Note
pad lowestNote chord =
  templateNotes
    1
    lowestNote
    chord
    (List.range 0 (List.length chord.flavor))

strum : Int -> Chord -> List Note
strum lowestNote chord =
  let
    strumChord =
      if List.length chord.flavor <= 2 then
        { chord | flavor = chord.flavor ++ [ 12 ] }
      else
        chord
  in
    pad lowestNote strumChord

intro : Int -> Chord -> List Note
intro lowestNote chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      default lowestNote chord
    Just scheme ->
      templateNotes 0.25 lowestNote chord scheme.intro

continuation : Int -> Chord -> List Note
continuation lowestNote chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      default lowestNote chord
    Just scheme ->
      templateNotes 0.25 lowestNote chord scheme.continuation

default : Int -> Chord -> List Note
default lowestNote chord =
  templateNotes
    0.25
    lowestNote
    chord
    ( List.map
        (\i -> i % (1 + List.length chord.flavor))
        (List.range 0 15)
    )

templateNotes : Float -> Int -> Chord -> List Int -> List Note
templateNotes interval lowestNote chord codes =
  let
    rootPitch =
      (chord.root - lowestNote) % 12 + lowestNote
  in let
    chordFrequencies =
      List.map
        (pitchFrequency << (+) rootPitch)
        (0 :: chord.flavor)
  in
    List.concat
      ( List.indexedMap
          (codeNotes interval chordFrequencies)
          codes
      )

pitchFrequency : Int -> Float
pitchFrequency pitch =
  440 * 2 ^ (toFloat (pitch - 69) / 12)

codeNotes : Float -> List Float -> Int -> Int -> List Note
codeNotes interval chordFrequencies i code =
  List.map
    (Note (interval * toFloat i))
    (codeFrequencies chordFrequencies code)

codeFrequencies : List Float -> Int -> List Float
codeFrequencies chordFrequencies code =
  let
    lowFrequencies =
      if code == -1 then
        []
      else
        List.take 1 (List.drop (code % 10) chordFrequencies)
  in let
    octaves = (code // 10) % 10
  in let
    frequencies =
      List.map ((*) (toFloat (2 ^ octaves))) lowFrequencies
  in
    if code >= 100 then
      frequencies ++ codeFrequencies chordFrequencies (code // 100)
    else
      frequencies

type alias Scheme =
  { intro : List Int
  , continuation : List Int
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
    , ( [ 3, 7, 10, 14 ], minorNinth )
    , ( [ 4, 7, 10, 13 ], ninth )
    , ( [ 4, 7, 10, 14, 21 ], thirteenth )
    , ( [ 4, 7, 11, 14, 21 ], thirteenth )
    , ( [ 3, 7, 10, 14, 21 ], thirteenth )
    ]

triad : Scheme
triad =
  basic [ 2000, 1, 2, 10, 11, 12, 10, 11 ]

seventh : Scheme
seventh =
  basic [ 1300, 1, 2, 3, 10, 11, 3, 10 ]

thirteenth : Scheme
thirteenth =
  basic [ 500, 1, 2, 3, 4, 2, 3, 4 ]

addNinth : Scheme
addNinth =
  basic [ 1300, 1, 2, 3, 11, 12, 3, 11 ]

ninth : Scheme
ninth =
  basic [ 1400, 1, 2, 11, 12, 13, 11, 12 ]

minorNinth : Scheme
minorNinth =
  basic [ 1400, 1, 2, 10, 12, 13, 10, 12 ]

basic : List Int -> Scheme
basic half =
  { intro =
      0 :: List.drop 1 half ++ half
  , continuation =
      half ++ half
  }
