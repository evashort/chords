module AudioChange exposing (AudioChange(..), ChangeTime, perform)

import Note exposing (Note)
import Ports

import Json.Encode as Encode

type AudioChange
  = AddNote Note
  | MuteAllNotes ChangeTime
  | CancelFutureNotes ChangeTime
  | SetAttack Float
  | SetPeak Float
  | SetDecay Float

type alias ChangeTime =
  { t : Float
  , before : Bool
  }

perform : List AudioChange -> Cmd msg
perform = Ports.changeAudio << List.map toJson

toJson : AudioChange -> Encode.Value
toJson change =
  case change of
    AddNote { t, f } ->
      Encode.object
        [ ( "type", Encode.string "note" )
        , ( "t", Encode.float t )
        , ( "f", Encode.float f )
        ]
    MuteAllNotes { t, before } ->
      Encode.object
        [ ( "type", Encode.string "mute" )
        , ( "t", Encode.float t )
        , ( "before", Encode.bool before )
        ]
    CancelFutureNotes { t, before } ->
      Encode.object
        [ ( "type", Encode.string "cancel" )
        , ( "t", Encode.float t )
        , ( "before", Encode.bool before )
        ]
    SetAttack attack ->
      Encode.object
        [ ( "type", Encode.string "attack" )
        , ( "attack", Encode.float attack )
        ]
    SetPeak peak ->
      Encode.object
        [ ( "type", Encode.string "peak" )
        , ( "peak", Encode.float peak )
        ]
    SetDecay decay ->
      Encode.object
        [ ( "type", Encode.string "decay" )
        , ( "decay", Encode.float decay )
        ]
