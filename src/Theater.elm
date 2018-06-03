port module Theater exposing (Frame, init, replace, undoAndReplace, hardUndo)

import Ports
import Replacement exposing (Replacement)
import Substring

import Json.Encode as Encode

type alias Frame =
  { text : String
  , selectionStart : Int
  , selectionEnd : Int
  }

init : Frame -> Cmd msg
init frame =
  Ports.initTheater
    ( Encode.object
        [ ( "text", Encode.string frame.text )
        , ( "selectionStart", Encode.int frame.selectionStart )
        , ( "selectionEnd", Encode.int frame.selectionEnd )
        ]
    )

replace : Replacement -> Cmd msg
replace = Ports.replace << encodeReplacement

undoAndReplace : Replacement -> Cmd msg
undoAndReplace = Ports.undoAndReplace << encodeReplacement

hardUndo : Cmd msg
hardUndo =
  Ports.hardUndo ()

encodeReplacement : Replacement -> Encode.Value
encodeReplacement replacement =
  Encode.object
    [ ( "selectionStart", Encode.int replacement.old.i )
    , ( "selectionEnd", Encode.int (Substring.stop replacement.old) )
    , ( "text", Encode.string replacement.new )
    ]
