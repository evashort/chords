module CustomEvents exposing (onChange, onLeftDown, onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode exposing (Decoder)

onChange : (String -> msg) -> Attribute msg
onChange tag =
  on "change" (Decode.map tag targetValue)

onLeftDown : msg -> Attribute msg
onLeftDown message =
  on
    "mousedown"
    ( Decode.andThen
        (requireLeftButton message)
        (Decode.field "button" Decode.int)
    )

requireLeftButton : msg -> Int -> Decoder msg
requireLeftButton message button =
  case button of
    0 ->
      Decode.succeed message
    _ ->
      Decode.fail ("ignoring button " ++ toString button)

onKeyDown : List ( Int, msg ) -> Attribute msg
onKeyDown messageMap =
  on
    "keydown"
    ( Decode.andThen
        (pickMessage messageMap)
        (Decode.field "which" Decode.int)
    )

pickMessage : List ( Int, msg ) -> Int -> Decoder msg
pickMessage messageMap which =
  case messageMap of
    ( key, message ) :: rest ->
      if key == which then
        Decode.succeed message
      else
        pickMessage rest which
    [] ->
      Decode.fail ("ignoring key " ++ toString which)
