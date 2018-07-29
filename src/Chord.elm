module Chord exposing
  ( Chord, fromCode, fromCodeExtended, maxRange, transpose, toPitchSet
  , fromPitchSet, flavors
  )

import Pitch
import Submatches exposing (submatches)

import Dict exposing (Dict)
import Regex exposing (Regex)
import Set exposing (Set)

type alias Chord =
  { flavor : List Int
  , root : Int
  }

fromCode : String -> Maybe Chord
fromCode code =
  case submatches regex code of
    [ Just rootCode, Just flavorCode ] ->
      case Dict.get flavorCode flavors of
        Nothing ->
          Nothing
        Just flavor ->
          Just
            { flavor = flavor
            , root = Pitch.fromCode rootCode
            }
    _ ->
      Nothing

fromCodeExtended : String -> Maybe Chord
fromCodeExtended code =
  case fromCode code of
    Just chord ->
      Just chord
    Nothing ->
      let
        words =
          List.filter
            (not << String.isEmpty)
            (String.split " " code)
      in let
        pitches = List.filterMap stringToInt words
      in
        if List.length pitches < List.length words then
          Nothing
        else
          case removeDuplicates (List.sort pitches) of
            [] ->
              Nothing
            rootPitch :: flavorPitches ->
              let
                tallFlavor =
                  List.map ((+) -rootPitch) flavorPitches
              in
                Just
                  { flavor =
                      List.filter ((>=) maxRange) tallFlavor
                  , root = rootPitch % 12
                  }

stringToInt : String -> Maybe Int
stringToInt string =
  case String.toInt string of
    Ok int ->
      Just int
    Err _ ->
      Nothing

removeDuplicates : List a -> List a
removeDuplicates xs =
  case xs of
    x :: y :: rest ->
      if x == y then
        removeDuplicates (y :: rest)
      else
        x :: removeDuplicates (y :: rest)
    other ->
      other

maxRange : Int
maxRange = 23

regex : Regex
regex = Regex.regex "^([A-Ga-g][b#♭♯]?)(.*)"

flavors : Dict String (List Int)
flavors =
  Dict.fromList
    [ ( "", [ 4, 7 ] )
    , ( "m", [ 3, 7 ] )
    , ( "o", [ 3, 6 ] )
    , ( "+", [ 4, 8 ] )
    , ( "sus4", [ 5, 7 ] )
    , ( "sus2", [ 2, 7 ] )
    , ( "7", [ 4, 7, 10 ] )
    , ( "M7", [ 4, 7, 11 ] )
    , ( "m7", [ 3, 7, 10 ] )
    , ( "6", [ 4, 7, 9 ] )
    , ( "0", [ 3, 6, 10 ] )
    , ( "m6", [ 3, 7, 9 ] )
    , ( "o7", [ 3, 6, 9 ] )
    , ( "mM7", [ 3, 7, 11 ] )
    , ( "9", [ 4, 7, 10, 14 ] ) -- 7 + 0
    , ( "M9", [ 4, 7, 11, 14 ] ) -- M7 + m7
    , ( "add9", [ 4, 7, 14 ] )
    , ( "madd9", [ 3, 7, 14 ] )
    , ( "m9", [ 3, 7, 10, 14 ] ) -- m7 + M7
    , ( "7b9", [ 4, 7, 10, 13 ] ) -- 7 + o7
    , ( "13", [ 4, 7, 10, 14, 21 ] )
    , ( "M13", [ 4, 7, 11, 14, 21 ] )
    , ( "m13", [ 3, 7, 10, 14, 21 ] )
    ]

transpose : Int -> Chord -> Chord
transpose offset chord =
  { chord | root = (chord.root + offset) % 12 }

toPitchSet : Int -> Int -> Maybe Chord -> Set Int
toPitchSet lowestPitch octave maybeChord =
  case maybeChord of
    Nothing ->
      Set.empty
    Just chord ->
      let
        rootPitch =
          (chord.root - lowestPitch) % 12 +
            lowestPitch +
            12 * octave
      in
        Set.fromList
          (List.map ((+) rootPitch) (0 :: chord.flavor))

fromPitchSet : Int -> Set Int -> Maybe (Chord, Int)
fromPitchSet lowestPitch pitchSet =
  case Set.toList pitchSet of
    [] ->
      Nothing
    rootPitch :: flavorPitches ->
      Just
        ( Chord
            (List.map ((+) -rootPitch) flavorPitches)
            (rootPitch % 12)
        , ( rootPitch - lowestPitch -
              (rootPitch - lowestPitch) % 12
          ) //
            12
        )
