port module Ports exposing (undo, redo)

port undo : (() -> msg) -> Sub msg

port redo : (() -> msg) -> Sub msg
