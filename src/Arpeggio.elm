module Arpeggio exposing (startLow, startHigh)

import Chord exposing (Chord)
import Clip exposing (Clip)
import Sound

import Dict exposing (Dict)

startLow : Int -> Chord -> Clip
startLow lowestPitch chord =
  let
    scheme =
      case Dict.get chord.flavor schemes of
        Nothing ->
          Debug.todo
            ( "Arpeggio.startLow: Unknown flavor " ++
                Debug.toString chord.flavor
            )
        Just someScheme ->
          0 :: List.drop 1 someScheme
    pitches = Chord.toPitches lowestPitch chord
    times = List.map ((*) 0.25 << toFloat) (List.range 0 8)
  in
    { sounds =
        List.concat
          ( List.map2
              (List.map << Sound.piano)
              times
              (List.map (schemeChord pitches) scheme)
          )
    , stop = 2
    }

startHigh : Int -> Chord -> Clip
startHigh lowestPitch chord =
  let
    scheme =
      case Dict.get chord.flavor schemes of
        Nothing ->
          Debug.todo
            ( "Arpeggio.startLow: Unknown flavor " ++
                Debug.toString chord.flavor
            )
        Just someScheme ->
          someScheme
    pitches = Chord.toPitches lowestPitch chord
    times = List.map ((*) 0.25 << toFloat) (List.range 0 8)
  in
    { sounds =
        List.concat
          ( List.map2
              (List.map << Sound.piano)
              times
              (List.map (schemeChord pitches) scheme)
          )
    , stop = 2
    }

schemeChord : List Int -> Int -> List Int
schemeChord chordPitches schemeNumber =
  if schemeNumber == -1 then
    []
  else
    let
      basePitch =
        case List.drop (modBy 10 schemeNumber) chordPitches of
          [] ->
            Debug.todo
              ( "Arpeggio.schemeChord: Bad schemeNumber: " ++
                  Debug.toString schemeNumber
              )
          chordPitch :: _ ->
            chordPitch
      octave = modBy 10 (schemeNumber // 10)
      rest =
        if schemeNumber >= 100 then
          schemeChord chordPitches (schemeNumber // 100)
        else
          []
    in
      basePitch + 12 * octave :: rest

schemes : Dict (List Int) (List Int)
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
    , ( "6/9", sixSlashNine )
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

triad : List Int
triad = [ 2000, 1, 2, 10, 11, 12, 10, 11 ]

seventh : List Int
seventh = [ 1300, 1, 2, 3, 10, 11, 3, 10 ]

thirteenth : List Int
thirteenth = [ 1300, 1, 2, 3, 4, 5, 3, 4 ]

addNinth : List Int
addNinth = [ 1300, 1, 2, 3, 11, 12, 3, 11 ]

ninth : List Int
ninth = [ 1400, 1, 2, 11, 12, 13, 11, 12 ]

minorNinth : List Int
minorNinth = [ 1400, 1, 2, 10, 12, 13, 10, 12 ]

sixSlashNine : List Int
sixSlashNine = [ 1400, 1, 2, 4, 11, 13, 4, 11 ]

eleventh : List Int
eleventh = [ 1300, 1, 2, 3, 4, 5, 3, 4 ]

addEleventh : List Int
addEleventh = [ 2000, 1, 2, 10, 11, 3, 10, 11 ]
