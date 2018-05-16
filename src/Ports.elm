port module Ports exposing (undoPort, redoPort)

port undo : (() -> msg) -> Sub msg

port redo : (() -> msg) -> Sub msg
