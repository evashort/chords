module ChordFromCode exposing (chordFromCode)

import Chord exposing (Chord)

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..), Match)

chordFromCode : String -> Maybe Chord
chordFromCode code =
  case partsFromCode code of
    Nothing ->
      Nothing
    Just ( chordCode, Nothing ) ->
      fromCodeGivenOctave 2 chordCode
    Just ( chordCode, Just rootCode ) ->
      case rootFromCode rootCode of
        Nothing ->
          case String.toInt rootCode of
            Err _ ->
              Nothing
            Ok octave ->
              fromCodeGivenOctave octave chordCode
        Just root ->
          case fromCodeGivenOctave 2 chordCode of
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
fromCodeGivenOctave octave code =
  case noteAndSomethingFromCode code of
    Nothing ->
      Nothing
    Just ( pitch, flavor ) ->
      case Dict.get (normalizeFlavor flavor) flavorOffsets of
        Nothing ->
          Nothing
        Just offsets ->
          Just (List.map ((+) (12 * (octave + 2) + pitch)) offsets)

rootFromCode : String -> Maybe Int
rootFromCode code =
  case noteAndSomethingFromCode code of
    Nothing ->
      Nothing
    Just ( pitch, "" ) ->
      Just (48 + pitch)
    Just ( pitch, octaveName ) ->
      case String.toInt octaveName of
        Err _ ->
          Nothing
        Ok octave ->
          Just (12 * (octave + 2) + pitch)

noteAndSomethingFromCode : String -> Maybe ( Int, String )
noteAndSomethingFromCode code =
  case Regex.find (AtMost 1) noteAndSomething code of
    [ match ] ->
      case match.submatches of
        [ Just letter, Just accidental, Just something ] ->
          Just
            ( (letterPitch letter + accidentalValue accidental) % 12
            , something
            )
        _ ->
          Nothing
    _ ->
      Nothing

noteAndSomething : Regex
noteAndSomething = Regex.regex "^([A-Ga-g])([𝄫♭b#♯x*𝄪ˣ×]*)(.*)$"

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

letterPitch : String -> Int
letterPitch letter =
  case String.toUpper letter of
    "C" -> 0
    "D" -> 2
    "E" -> 4
    "F" -> 5
    "G" -> 7
    "A" -> 9
    _ -> 11

accidentalValue : String -> Int
accidentalValue accidental =
  List.sum
    ( List.map
        (Maybe.withDefault 0 << (flip Dict.get) accidentalCharValues)
        ( String.toList
            (Regex.replace All accidentalWords toAccidentalSymbol accidental)
        )
    )

accidentalWords : Regex
accidentalWords = Regex.regex "𝄫|𝄪"

toAccidentalSymbol : Match -> String
toAccidentalSymbol match =
  case match.match of
    "𝄫" -> "bb"
    "𝄪" -> "x"
    _ -> match.match

accidentalCharValues : Dict Char Int
accidentalCharValues =
  Dict.fromList
    [ ( '♭', -1 ), ( 'b', -1 )
    , ( '#', 1 ), ( '♯', 1 )
    , ( 'x', 2 ), ( '*', 2 ), ( 'ˣ', 2 ), ( '×', 2 )
    ]

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
