port module Selection exposing ( fromSubstring, set, check, receive )

import Substring exposing (Substring)

fromSubstring : Substring -> ( Int, Int )
fromSubstring substring =
  ( substring.i, Substring.stop substring )

set : ( Int, Int ) -> Cmd msg
set = setSelection
port setSelection : ( Int, Int ) -> Cmd msg

check : Cmd msg
check = checkSelection ()
port checkSelection : () -> Cmd msg

receive : (( Int, Int ) -> msg) -> Sub msg
receive = receiveSelection
port receiveSelection : (( Int, Int ) -> msg) -> Sub msg
