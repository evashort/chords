module ChordFromCode exposing (chordFromCode)

import Chord exposing (Chord)
import NoteParser

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..), Match)

chordFromCode : Int -> String -> Maybe Chord
chordFromCode lowestNote code =
  case partsFromCode code of
    Nothing ->
      Nothing
    Just ( chordCode, Nothing ) ->
      fromCodeGivenOctave lowestNote chordCode
    Just ( chordCode, Just rootCode ) ->
      case rootFromCode lowestNote rootCode of
        Nothing ->
          case String.toInt rootCode of
            Err _ ->
              Nothing
            Ok octave ->
              fromCodeGivenOctave (12 * (octave + 2)) chordCode
        Just root ->
          case fromCodeGivenOctave lowestNote chordCode of
            Nothing ->
              Nothing
            Just namesakeChord ->
              case Chord.indexOf root namesakeChord of
                [] ->
                  Nothing
                i :: _ ->
                  Just (Chord.invert i namesakeChord)

partsFromCode : String -> Maybe ( String, Maybe String )
partsFromCode code =
  case String.split "/" (String.map normalizeSeparatorChar code) of
    [ chordCode, rootCode ] ->
      Just ( chordCode, Just rootCode )
    [ chordCode ] ->
      Just ( chordCode, Nothing )
    _ ->
      Nothing

normalizeSeparatorChar : Char -> Char
normalizeSeparatorChar char =
  case char of
    '⁄' -> '/'
    _ -> char

fromCodeGivenOctave : Int -> String -> Maybe Chord
fromCodeGivenOctave octaveStart code =
  case NoteParser.parseAtStart code of
    Nothing ->
      Nothing
    Just ( pitch, flavor ) ->
      case Dict.get (normalizeFlavor flavor) flavorOffsets of
        Nothing ->
          Nothing
        Just offsets ->
          Just
            ( List.map
                ((+) (octaveStart + (pitch - octaveStart) % 12))
                offsets
            )

rootFromCode : Int -> String -> Maybe Int
rootFromCode lowestNote code =
  case NoteParser.parseAtStart code of
    Nothing ->
      Nothing
    Just ( pitch, "" ) ->
      let relativePitch = pitch - lowestNote in
        Just (lowestNote + (pitch - lowestNote) % 12)
    Just ( pitch, octaveName ) ->
      case String.toInt octaveName of
        Err _ ->
          Nothing
        Ok octave ->
          Just (12 * (octave + 2) + pitch)

normalizeFlavor : String -> String
normalizeFlavor =
  String.filter notParen <<
    Regex.replace All flavorWords toFlavorSymbol <<
      String.map normalizeFlavorChar

notParen : Char -> Bool
notParen char =
  char /= '(' && char /= ')'

normalizeFlavorChar : Char -> Char
normalizeFlavorChar char =
  Maybe.withDefault char (Dict.get char flavorCharNormalizations)

flavorCharNormalizations : Dict Char Char
flavorCharNormalizations =
  Dict.fromList
    [ ( 'ᴹ', 'M' ), ( 'ᵐ', 'm' ), ( '-', 'm' ), ( '⁻', 'm' ), ( '−', 'm' )
    , ( '⁷', '7' ), ( '⁺', '+' ), ( '⁽', '(' ), ( '⁾', ')' )
    , ( '♭', 'b' ), ( '♭', 'b' ), ( 'ᵇ', 'b' ), ( '♯', '#' )
    , ( '²', '2' ), ( '⁴', '4' ), ( '⁵', '5' ), ( '⁶', '6' )
    , ( '⁰', 'o' ), ( '°', 'o' ), ( 'º', 'o' ), ( 'ᵒ', 'o' )
    , ( 'ᴼ', 'o' ), ( 'Ø', '0' ), ( '∅', '0' ), ( 'ø', '0' )
    , ( 'ᵃ', 'a' ), ( 'ª', 'a'), ('ʲ', 'j'), ( 'ⁱ', 'i' )
    , ( 'ˢ', 's' ), ( 'ᵘ', 'u' ), ( 'ᶸ', 'u' )
    , ('ⁿ', 'n'), ('ᵈ', 'd'), ('ᵍ', 'g')
    ]

flavorWords : Regex
flavorWords = Regex.regex "maj|min|dim|aug"

toFlavorSymbol : Match -> String
toFlavorSymbol match =
  case match.match of
    "maj" -> "M"
    "min" -> "m"
    "dim" -> "o"
    "aug" -> "+"
    _ -> match.match

flavorOffsets : Dict String (List Int)
flavorOffsets =
  Dict.fromList
    [ ( "m", [ 0, 3, 7 ] )
    , ( "o", [ 0, 3, 6 ] )
    , ( "+", [ 0, 4, 8 ] )
    , ( "7", [ 0, 4, 7, 10 ] )
    , ( "7b5", [ 0, 4, 6, 10 ] )

    , ( "M", [ 0, 4, 7 ] )
    , ( "", [ 0, 4, 7 ] )

    , ( "sus4", [ 0, 5, 7 ] )
    , ( "sus2", [ 0, 2, 7 ] )

    , ( "m7", [ 0, 3, 7, 10 ] )
    , ( "6", [ 0, 4, 7, 9 ] )
    , ( "M6", [ 0, 4, 7, 9 ] )

    , ( "0", [ 0, 3, 6, 10 ] )
    , ( "07", [ 0, 3, 6, 10 ] )
    , ( "m7b5", [ 0, 3, 6, 10 ] )
    , ( "mM6", [ 0, 3, 7, 9 ] )
    , ( "m6", [ 0, 3, 7, 9 ] )

    , ( "M7", [ 0, 4, 7, 11 ] )
    , ( "Δ7", [ 0, 4, 7, 11 ] )
    , ( "Δ", [ 0, 4, 7, 11 ] )

    , ( "o7", [ 0, 3, 6, 9 ] )
    , ( "mb7b5", [ 0, 3, 6, 9 ] )

    , ( "mM7", [ 0, 3, 7, 11 ] )
    , ( "mΔ7", [ 0, 3, 7, 11 ] )

    , ( "+M7", [ 0, 4, 8, 11 ] )
    , ( "+Δ7", [ 0, 4, 8, 11 ] )
    , ( "M7#5", [ 0, 4, 8, 11 ] )

    , ( "m+7", [ 0, 4, 8, 10 ] )
    , ( "+7", [ 0, 4, 8, 10 ] )
    , ( "7#5", [ 0, 4, 8, 10 ] )

    , ( "mM7b5", [ 0, 3, 6, 11 ] )
    , ( "mΔ7b5", [ 0, 3, 6, 11 ] )
    , ( "oM7", [ 0, 3, 6, 11 ] )
    ]
