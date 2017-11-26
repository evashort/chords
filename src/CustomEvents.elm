module CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)

onLeftDown : msg -> Attribute msg
onLeftDown message =
  on
    "mousedown"
    ( Json.Decode.andThen
        (requireLeftButton message)
        (Json.Decode.field "button" Json.Decode.int)
    )

onLeftClick : msg -> Attribute msg
onLeftClick message =
  on
    "click"
    ( Json.Decode.andThen
        (requireLeftButton message)
        (Json.Decode.field "button" Json.Decode.int)
    )

requireLeftButton : msg -> Int -> Decoder msg
requireLeftButton message button =
  case button of
    0 -> Json.Decode.succeed message
    _ -> Json.Decode.fail ("ignoring button " ++ toString button)

onKeyDown : List ( Int, msg ) -> Attribute msg
onKeyDown messageMap =
  on
    "keydown"
    ( Json.Decode.andThen
        (pickMessage messageMap)
        (Json.Decode.field "which" Json.Decode.int)
    )

pickMessage : List ( Int, msg ) -> Int -> Decoder msg
pickMessage messageMap which =
  case messageMap of
    ( key, message ) :: rest ->
      if key == which then
        Json.Decode.succeed message
      else
        pickMessage rest which
    [] ->
      Json.Decode.fail ("ignoring key " ++ toString which)
