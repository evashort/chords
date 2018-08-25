module CustomEvents exposing
  ( onChange
  , isAudioTimeButton, onClickWithAudioTime
  , isAudioTimeInput, onInputWithAudioTime, onIntInputWithAudioTime
  )

import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

onChange : (String -> msg) -> Attribute msg
onChange tag =
  on "change" (Decode.map tag targetValue)

isAudioTimeButton : Bool -> Attribute msg
isAudioTimeButton value =
  property "isAudioTimeButton" (Encode.bool value)

onClickWithAudioTime : (Float -> msg) -> Attribute msg
onClickWithAudioTime tag =
  on
    "clickWithAudioTime"
    ( Decode.map
        tag
        (Decode.field "detail" Decode.float)
    )

isAudioTimeInput : Bool -> Attribute msg
isAudioTimeInput value =
  property "isAudioTimeInput" (Encode.bool value)

onInputWithAudioTime : ((String, Float) -> msg) -> Attribute msg
onInputWithAudioTime tag =
  on
    "inputWithAudioTime"
    ( Decode.map
        tag
        ( Decode.field
            "detail"
            ( Decode.map2
                (,)
                (Decode.field "value" Decode.string)
                (Decode.field "audioTime" Decode.float)
            )
        )
    )

onIntInputWithAudioTime : Int -> ((Int, Float) -> msg) -> Attribute msg
onIntInputWithAudioTime current tag =
  on
    "inputWithAudioTime"
    ( Decode.map
        tag
        ( Decode.field
            "detail"
            ( Decode.map2
                (,)
                ( Decode.field
                    "value"
                    ( Decode.andThen
                        (requireDifferentInt current)
                        Decode.string
                    )
                )
                (Decode.field "audioTime" Decode.float)
            )
        )
    )

requireDifferentInt : Int -> String -> Decoder Int
requireDifferentInt current valueString =
  case String.toInt valueString of
    Ok value ->
      if value /= current then
        Decode.succeed value
      else
        Decode.fail ("ignoring current value " ++ valueString)
    Err _ ->
      Decode.fail ("ignroring non-integer value " ++ valueString)
