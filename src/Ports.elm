port module Ports exposing
  (initTheater, focusTheater, replace, undoAndReplace, hardUndo, text)

import Json.Encode as Encode

port initTheater : Encode.Value -> Cmd msg
port focusTheater : () -> Cmd msg
port replace : Encode.Value -> Cmd msg
port undoAndReplace : Encode.Value -> Cmd msg
port hardUndo : () -> Cmd msg

port text : (String -> msg) -> Sub msg
