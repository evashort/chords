module Arp exposing (defaultBpm, pad, strum, intro, continuation)

import Chord exposing (Chord)
import Note exposing (Note)

import Dict exposing (Dict)

defaultBpm : Float
defaultBpm = 85

pad : Int -> Chord -> List Note
pad lowestPitch chord =
  templateNotes
    1
    lowestPitch
    chord
    (List.range 0 (List.length chord.flavor))

strum : Int -> Chord -> List Note
strum lowestPitch chord =
  let
    strumChord =
      if List.length chord.flavor <= 2 then
        { chord | flavor = chord.flavor ++ [ 12 ] }
      else
        chord
  in
    pad lowestPitch strumChord

intro : Int -> Chord -> List Note
intro lowestPitch chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      default lowestPitch chord
    Just scheme ->
      templateNotes 0.25 lowestPitch chord scheme.intro

continuation : Int -> Chord -> List Note
continuation lowestPitch chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      default lowestPitch chord
    Just scheme ->
      templateNotes 0.25 lowestPitch chord scheme.continuation

default : Int -> Chord -> List Note
default lowestPitch chord =
  templateNotes
    0.25
    lowestPitch
    chord
    ( List.map
        (\i -> i % (1 + List.length chord.flavor))
        (List.range 0 15)
    )

templateNotes : Float -> Int -> Chord -> List Int -> List Note
templateNotes interval lowestPitch chord codes =
  let
    rootPitch =
      (chord.root - lowestPitch) % 12 + lowestPitch
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
    (Note 1 (interval * toFloat i))
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
    octave = (code // 10) % 10
  in let
    frequencies =
      List.map ((*) (toFloat (2 ^ octave))) lowFrequencies
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
  Chord.dict
    [ ( "", triad )
    , ( "m", triad )
    , ( "o", triad )
    , ( "7", seventh )
    , ( "M7", seventh )
    , ( "m7", seventh )
    , ( "0", seventh )
    , ( "o7", seventh )
    , ( "mM7", seventh )
    , ( "9", ninth )
    , ( "M9", ninth )
    , ( "m9", minorNinth )
    , ( "7b9", ninth )
    , ( "M7#11", eleventh )
    , ( "13", thirteenth )
    , ( "M13", thirteenth )
    , ( "add9", addNinth )
    , ( "madd9", addNinth )
    , ( "addb9", addNinth )
    , ( "add#11", addEleventh )
    , ( "6", seventh )
    , ( "m6", seventh )
    , ( "+", triad )
    , ( "sus4", triad )
    , ( "sus2", triad )
    ]

triad : Scheme
triad =
  basic [ 2000, 1, 2, 10, 11, 12, 10, 11 ]

seventh : Scheme
seventh =
  basic [ 1300, 1, 2, 3, 10, 11, 3, 10 ]

thirteenth : Scheme
thirteenth =
  basic [ 1300, 1, 2, 3, 4, 5, 3, 4 ]

addNinth : Scheme
addNinth =
  basic [ 1300, 1, 2, 3, 11, 12, 3, 11 ]

ninth : Scheme
ninth =
  basic [ 1400, 1, 2, 11, 12, 13, 11, 12 ]

minorNinth : Scheme
minorNinth =
  basic [ 1400, 1, 2, 10, 12, 13, 10, 12 ]

eleventh : Scheme
eleventh =
  basic [ 1300, 1, 2, 3, 4, 5, 3, 4 ]

addEleventh : Scheme
addEleventh =
  basic [ 2000, 1, 2, 10, 11, 3, 10, 11 ]

basic : List Int -> Scheme
basic half =
  { intro =
      0 :: List.drop 1 half ++ half
  , continuation =
      half ++ half
  }
