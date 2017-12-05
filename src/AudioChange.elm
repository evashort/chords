port module AudioChange exposing (Note, muteAllNotes, playNotes)

import Json.Decode
import Json.Encode

type AudioChange
  = NewNote Note
  | MuteLoudestNote ChangeTime
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

muteAllNotes : Float -> Cmd msg
muteAllNotes now =
  changeAudio [ toJson (MuteAllNotes { t = now, before = False }) ]

playNotes : Float -> Bool -> Float -> List Note -> Cmd msg
playNotes decay mute now notes =
  case notes of
    [] ->
      Cmd.none
    note :: _ ->
      let
        cutoffChanges =
          if now + latency >= note.t then
            let notBeforeNote = max now note.t in
              if mute then
                [ MuteAllNotes { t = notBeforeNote, before = False } ]
              else
                [ CancelFutureNotes { t = notBeforeNote, before = False }
                , MuteLoudestNote { t = notBeforeNote, before = False }
                ]
          else
              [ (if mute then MuteAllNotes else CancelFutureNotes)
                  { t = note.t, before = True }
              ]
      in let
        noteChanges = List.map (NewNote << notBefore now) notes
      in
        changeAudio
          ( List.map
              toJson
              (cutoffChanges ++ SetDecay decay :: noteChanges)
          )

latency : Float
latency = 0.01

notBefore : Float -> Note -> Note
notBefore t note =
  if note.t >= t then note
  else { note | t = t }

port changeAudio : List Json.Encode.Value -> Cmd msg

toJson : AudioChange -> Json.Encode.Value
toJson change =
  case change of
    NewNote { t, f } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "note" )
        , ( "t", Json.Encode.float t )
        , ( "f", Json.Encode.float f )
        ]
    MuteLoudestNote { t, before } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "muteLoudest" )
        , ( "t", Json.Encode.float t )
        , ( "before", Json.Encode.bool before )
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
