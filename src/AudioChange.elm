port module AudioChange exposing (AudioChange(..), Note, ChangeTime, perform)

import Json.Decode
import Json.Encode

type AudioChange
  = AddNote Note
  | MuteAllNotes ChangeTime
  | CancelFutureNotes ChangeTime
  | SetDecay Float

type alias Note =
  { t : Float
  , f : Float
  }

type alias ChangeTime =
  { t : Float
  , before : Bool
  }

perform : List AudioChange -> Cmd msg
perform = changeAudio << List.map toJson

port changeAudio : List Json.Encode.Value -> Cmd msg

toJson : AudioChange -> Json.Encode.Value
toJson change =
  case change of
    AddNote { t, f } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "note" )
        , ( "t", Json.Encode.float t )
        , ( "f", Json.Encode.float f )
        ]
    MuteAllNotes { t, before } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "mute" )
        , ( "t", Json.Encode.float t )
        , ( "before", Json.Encode.bool before )
        ]
    CancelFutureNotes { t, before } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "cancel" )
        , ( "t", Json.Encode.float t )
        , ( "before", Json.Encode.bool before )
        ]
    SetDecay decay ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "decay" )
        , ( "decay", Json.Encode.float decay )
        ]
