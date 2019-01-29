module StrumPattern exposing (StrumPattern(..), basic, indie, modern)

import Chord exposing (Chord)
import Clip exposing (Clip)
import Sound exposing (Sound, and, up, down)

import Dict exposing (Dict)

type StrumPattern
  = Basic
  | Indie
  | Modern

basic : Int -> Chord -> Clip
basic = getClip 0.25 basicRhythm

indie : Int -> Chord -> Clip
indie = getClip 0.25 indieRhythm

modern : Int -> Chord -> Clip
modern = getClip 0.25 modernRhythm

getClip :
  Float -> List (Float -> List Int -> List Sound) -> Int -> Chord -> Clip
getClip interval rhythm lowestPitch chord =
  let
    rootPitch =
      modBy 12 (chord.root - lowestPitch) + lowestPitch
  in
    let
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
      chordPitches =
        List.map ((+) rootPitch) (0 :: chord.flavor)
      times =
        List.map
          ((*) interval << toFloat)
          (List.range 0 (List.length rhythm - 1))
    in
      { sounds =
          List.concatMap
            ( (|>)
                (List.map (getStringPitch chordPitches) strings)
            )
            (List.map2 (<|) rhythm times)
      , stop = interval * toFloat (List.length rhythm)
      }

getStringPitch : List Int -> Int -> Int
getStringPitch chordPitches string =
  let
    basePitch =
      case List.drop (abs (remainderBy 10 string)) chordPitches of
        chordPitch :: _ ->
          chordPitch
        [] ->
          Debug.todo
            ( "StrumPattern.getStringPitch: Bad string: " ++
                Debug.toString string
            )
    octave = string // 10
  in
    basePitch + 12 * octave

type alias Scheme =
  { low : List Int -- B2 and below
  , mid : List Int -- C3 to Eb3
  , high : List Int -- E3 and above
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
    , ( "m9", ninth )
    , ( "7b9", ninth )
    , ( "6/9", sixSlashNine )
    , ( "M7#11", thirteenth )
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

sixSlashNine : Scheme
sixSlashNine =
  { low = [ 0, 3, 4, 11, 12 ]
  , mid = [ 0, 1, 2, 3, 4 ]
  , high = [ -10, -12, 0, 1, 3, 4 ]
  }

addEleventh : Scheme
addEleventh =
  { low = [ 0, 2, 10, 11, 3 ]
  , mid = [ 0, 2, 10, 11, 3 ]
  , high = [ -10, -12, 0, 1, -13, 10 ]
  }

thirteenth : Scheme
thirteenth =
  { low = [ 0, 1, 3, 4, 5 ]
  , mid = [ 0, 1, 3, 4, 5 ]
  , high = [ -10, -12, 1, 3, 4, 5 ]
  }

basicRhythm : List (Float -> List Int -> List Sound)
basicRhythm =
  [ down 92 0, and,        down 81 0.09, up 92 0.13
  , and,       up 70 1.06, down 81 0.09, up 78 1
  ]

indieRhythm : List (Float -> List Int -> List Sound)
indieRhythm =
  [ down 85 0,  and, down 62 0, and
  , down 108 0, and, down 67 0, up 77 1.38
  ]

modernRhythm : List (Float -> List Int -> List Sound)
modernRhythm =
  [ down 99 0.07, and,        and,       and
  , down 92 0,    and,        and,       up 77 0.29
  , down 66 0.21, up 81 1.32, down 81 0, and
  , down 99 0,    and,        down 81 0, up 81 1.32
  ]
