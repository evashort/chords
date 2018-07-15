module Lowest exposing (flag, rangeStart, fromPitch, pitch, viewDefault, view)

import Flag exposing (Flag)
import Pitch
import Scale exposing (Scale)

import Html exposing (Html, text, sup)
import Regex exposing (Regex)

flag : Flag (Maybe Int)
flag =
  { key = "lowest"
  , fromCode = fromCode
  , code = code
  , default = Nothing
  }

fromCode : String -> Maybe (Maybe Int)
fromCode code =
  if String.toLower code == "default" then
    Just Nothing
  else if Regex.contains regex code then
    let
      letterPitch =
        case String.right 1 code of
          "1" -> 0
          "2" -> 2
          "3" -> 4
          "4" -> 5
          "5" -> 7
          "6" -> 9
          "7" -> 11
          x ->
            Debug.crash ("Lowest.fromCode: Unknown degree: " ++ x)
    in let
      accidentalOffset =
        case String.dropRight 1 code of
          "" -> 0
          "b" -> -1
          "#" -> 1
          "♭" -> -1
          "♯" -> 1
          x ->
            Debug.crash ("Lowest.fromCode: Unknown accidental: " ++ x)
    in
      Just (Just ((letterPitch + accidentalOffset) % 12))
  else
    Nothing

regex : Regex
regex = Regex.regex "^[b#♭♯]?[1-7]$"

code : Maybe Int -> String
code maybeLowest =
  case maybeLowest of
    Nothing ->
      "default"
    Just pitch ->
      let
        degreeIndex = (pitch * 7 + 6) // 12
      in let
        degreePitch = (degreeIndex * 12 + 5) // 7
      in
        case pitch - degreePitch of
          -1 ->
            "b" ++ toString (1 + degreeIndex)
          0 ->
            toString (1 + degreeIndex)
          _ ->
            Debug.crash
              ("Lowest.code: Lowest caused error: " ++ toString pitch)

rangeStart : Int
rangeStart = 39

defaultPitch : Int
defaultPitch = 45

fromPitch : Int -> Int -> Int
fromPitch tonic pitch =
  (pitch - tonic) % 12

pitch : Int -> Maybe Int -> Int
pitch tonic maybeLowest =
  case maybeLowest of
    Nothing ->
      defaultPitch
    Just lowest ->
      (tonic + lowest - rangeStart) % 12 + rangeStart

viewDefault : Scale -> List (Html msg)
viewDefault scale =
  view scale (fromPitch scale.tonic defaultPitch)

view : Scale -> Int -> List (Html msg)
view scale lowest =
  let
    degreeIndex = (lowest * 7 + 6) // 12
  in let
    degreePitch = (degreeIndex * 12 + 5) // 7
  in let
    accidentalString =
      case lowest - degreePitch of
        -1 ->
          "♭"
        0 ->
          ""
        _ ->
          Debug.crash
            ("Lowest.view: Lowest caused error: " ++ toString lowest)
  in let
    degreeString =
      case degreeIndex of
        0 ->
          if scale.minor then "III" else "I"
        1 ->
          if scale.minor then "iv" else "ii"
        2 ->
          if scale.minor then "v" else "iii"
        3 ->
          if scale.minor then "VI" else "IV"
        4 ->
          if scale.minor then "VII" else "V"
        5 ->
          if scale.minor then "i" else "vi"
        6 ->
          if scale.minor then "ii" else "vii"
        _ ->
          Debug.crash
            ("Lowest.view: Lowest caused error: " ++ toString lowest)
  in let
    pitch =
      (scale.tonic + lowest - rangeStart) % 12 + rangeStart
  in let
    sharpCount =
      case scale.tonic of
        7 -> 1
        2 -> 2
        9 -> 3
        4 -> 4
        11 -> 5
        _ -> 0
  in let
    pitchString = Pitch.view sharpCount (pitch % 12)
  in
    if degreeIndex == 6 then
      [ text (accidentalString ++ degreeString)
      , sup [] [ text "o" ]
      , text (" / " ++ pitchString)
      ]
    else
      [ text
          ( String.concat
              [ accidentalString
              , degreeString
              , " / "
              , pitchString
              ]
          )
      ]
