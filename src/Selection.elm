port module Selection exposing
  ( Selection, fromSubstring, set, check, receive )

import Substring exposing (Substring)

type alias Selection =
  { start : Int
  , stop : Int
  }

fromSubstring : Substring -> Selection
fromSubstring substring =
  { start = substring.i
  , stop = Substring.stop substring
  }

set : Selection -> Cmd msg
set = setSelection
port setSelection : Selection -> Cmd msg

check : Cmd msg
check = checkSelection ()
port checkSelection : () -> Cmd msg

receive : (Selection -> msg) -> Sub msg
receive = receiveSelection
port receiveSelection : (Selection -> msg) -> Sub msg
