port module Selection exposing
  (setSelection, checkSelection, receiveSelection)

port setSelection : ( Int, Int ) -> Cmd msg

port checkSelection : () -> Cmd msg

port receiveSelection : (( Int, Int ) -> msg) -> Sub msg
