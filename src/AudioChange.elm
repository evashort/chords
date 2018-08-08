module AudioChange exposing (AudioChange(..), perform)

import Note exposing (Note)
import Ports

import Json.Encode as Encode

type AudioChange
  = AddPianoNote Note
  | AddGuitarNote Note
  | AddPadNote Note
  | NoteOff Note
  | Mute Float
  | Cancel Float

perform : List AudioChange -> Cmd msg
perform = Ports.changeAudio << List.map toJson

toJson : AudioChange -> Encode.Value
toJson change =
  case change of
    AddPianoNote { v, t, f } ->
      Encode.object
        [ ( "type", Encode.string "piano" )
        , ( "v", Encode.float v )
        , ( "t", Encode.float t )
        , ( "f", Encode.float f )
        ]
    AddGuitarNote { v, t, f } ->
      Encode.object
        [ ( "type", Encode.string "guitar" )
        , ( "v", Encode.float v )
        , ( "t", Encode.float t )
        , ( "f", Encode.float f )
        ]
    AddPadNote { v, t, f } ->
      Encode.object
        [ ( "type", Encode.string "pad" )
        , ( "v", Encode.float v )
        , ( "t", Encode.float t )
        , ( "f", Encode.float f )
        ]
    NoteOff { t, f } ->
      Encode.object
        [ ( "type", Encode.string "noteOff" )
        , ( "t", Encode.float t )
        , ( "f", Encode.float f )
        ]
    Mute t ->
      Encode.object
        [ ( "type", Encode.string "mute" )
        , ( "t", Encode.float t )
        ]
    Cancel t ->
      Encode.object
        [ ( "type", Encode.string "cancel" )
        , ( "t", Encode.float t )
        ]
