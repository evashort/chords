port module Selection exposing ( set, check, receive )

set : ( Int, Int ) -> Cmd msg
set = setSelection
port setSelection : ( Int, Int ) -> Cmd msg

check : Cmd msg
check = checkSelection ()
port checkSelection : () -> Cmd msg

receive : (( Int, Int ) -> msg) -> Sub msg
receive = receiveSelection
port receiveSelection : (( Int, Int ) -> msg) -> Sub msg
