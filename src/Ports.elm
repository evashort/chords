port module Ports exposing
  ( changeAudio, stopped
  , initTheater, focusTheater, replace, undoAndReplace, hardUndo
  , text
  )

import Json.Encode as Encode

port changeAudio : List Encode.Value -> Cmd msg
port stopped : (() -> msg) -> Sub msg

port initTheater : Encode.Value -> Cmd msg
port focusTheater : () -> Cmd msg
port replace : Encode.Value -> Cmd msg
port undoAndReplace : Encode.Value -> Cmd msg
port hardUndo : () -> Cmd msg

port text : (String -> msg) -> Sub msg
