module Arp exposing (strum, intro, continuation)

import Chord exposing (Chord)
import Note exposing (Note)

import Dict exposing (Dict)

strum : Int -> Chord -> List Note
strum lowestNote chord =
  templateNotes
    1
    lowestNote
    chord
    (List.range 0 (List.length chord.flavor) ++ [ 10 ])

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
        ((%) (1 + List.length chord.flavor))
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
    ]

triad : Scheme
triad =
  { intro =
      [    0, 1, 2, 10, 11, 12, 10, 11
      , 2000, 1, 2, 10, 11, 12, 10, 11
      ]
  , continuation =
      [ 2000, 1, 2, 10, 11, 12, 10, 11
      , 2000, 1, 2, 10, 11, 12, 10, 11
      ]
  }

seventh : Scheme
seventh =
  { intro =
      [    0, 1, 2, 3, 10, 11, 3, 10
      , 1200, 1, 2, 3, 10, 11, 3, 10
      ]
  , continuation =
      [ 2000, 1, 2, 3, 10, 11, 3, 10
      , 1200, 1, 2, 3, 10, 11, 3, 10
      ]
  }
