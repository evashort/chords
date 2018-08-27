module Lowest exposing
  (flag, rangeStart, fromPitch, toPitch, viewDefault, view)

import Flag exposing (Flag)
import Pitch
import Scale exposing (Scale)
import SharpCount

import Html exposing (Html, text, sup)
import Regex exposing (Regex)

flag : Flag (Maybe Int)
flag =
  { key = "lowest"
  , fromCode = fromCode
  , toCode = toCode
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
            Debug.todo ("Lowest.fromCode: Unknown degree: " ++ x)
      accidentalOffset =
        case String.dropRight 1 code of
          "" -> 0
          "b" -> -1
          "#" -> 1
          "♭" -> -1
          "♯" -> 1
          x ->
            Debug.todo ("Lowest.fromCode: Unknown accidental: " ++ x)
    in
      Just (Just (modBy 12 (letterPitch + accidentalOffset)))
  else
    Nothing

regex : Regex
regex =
  Maybe.withDefault Regex.never (Regex.fromString "^[b#♭♯]?[1-7]$")

toCode : Maybe Int -> String
toCode maybeLowest =
  case maybeLowest of
    Nothing ->
      "default"
    Just pitch ->
      let
        degreeIndex = (pitch * 7 + 6) // 12
      in
      let
        degreePitch = (degreeIndex * 12 + 5) // 7
      in
        case pitch - degreePitch + 1 of -- elm can't handle negative cases
          0 ->
            "b" ++ String.fromInt (1 + degreeIndex)
          1 ->
            String.fromInt (1 + degreeIndex)
          _ ->
            Debug.todo
              ( "Lowest.code: Lowest caused error: " ++
                  Debug.toString pitch
              )

rangeStart : Int
rangeStart = 39

defaultPitch : Int
defaultPitch = 45

fromPitch : Int -> Int -> Int
fromPitch tonic pitch =
  modBy 12 (pitch - tonic)

toPitch : Int -> Maybe Int -> Int
toPitch tonic maybeLowest =
  case maybeLowest of
    Nothing ->
      defaultPitch
    Just lowest ->
      modBy 12 (tonic + lowest - rangeStart) + rangeStart

viewDefault : Scale -> List (Html msg)
viewDefault scale =
  view scale (fromPitch scale.tonic defaultPitch)

view : Scale -> Int -> List (Html msg)
view scale lowest =
  let
    degreeIndex = (lowest * 7 + 6) // 12
  in
  let
    degreePitch = (degreeIndex * 12 + 5) // 7
  in
  let
    accidentalString =
      case lowest - degreePitch + 1 of -- elm can't handle negative cases
        0 ->
          "♭"
        1 ->
          ""
        _ ->
          Debug.todo
            ( "Lowest.view: Lowest caused error: " ++
                Debug.toString lowest
            )
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
          Debug.todo
            ( "Lowest.view: Lowest caused error: " ++
                Debug.toString lowest
            )
    pitch =
      modBy 12 (scale.tonic + lowest - rangeStart) + rangeStart
    sharpCount = SharpCount.fromTonic scale.tonic
  in
  let
    pitchString = Pitch.view sharpCount (modBy 12 pitch)
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
