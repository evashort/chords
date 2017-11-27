port module AudioChange exposing (muteAllNotes, playNotes)

import Json.Decode
import Json.Encode

type AudioChange
  = NewNote Note
  | MuteLoudestNote ChangeTime
  | MuteAllNotes ChangeTime
  | CancelFutureNotes ChangeTime

type alias Note =
  { decay : Float
  , t : Float
  , f : Float
  }

type alias ChangeTime =
  { t : Float
  , before : Bool
  }

muteAllNotes : Float -> Cmd msg
muteAllNotes now =
  changeAudio [ toJson (MuteAllNotes { t = now, before = False }) ]

playNotes :
  Float -> Bool -> Float -> List Float -> List (List Int) -> Cmd msg
playNotes decay mute now ts chords =
  case ts of
    [] ->
      Cmd.none
    t :: _ ->
      let
        cutoffChanges =
          if now + latency >= t then
            let notBeforeNote = max now t in
              if mute then
                [ MuteAllNotes { t = notBeforeNote, before = False } ]
              else
                [ CancelFutureNotes { t = notBeforeNote, before = False }
                , MuteLoudestNote { t = notBeforeNote, before = False }
                ]
          else
              [ (if mute then MuteAllNotes else CancelFutureNotes)
                  { t = t, before = True }
              ]
      in let
        noteChanges =
          List.concat (List.map2 (toNoteChanges decay now) ts chords)
      in
        changeAudio (List.map toJson (cutoffChanges ++ noteChanges))

latency : Float
latency = 0.01

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

toNoteChanges : Float -> Float -> Float -> List Int -> List AudioChange
toNoteChanges decay now t chord =
  List.map (NewNote << Note decay (max now t) << mtof) chord

port changeAudio : List Json.Encode.Value -> Cmd msg

toJson : AudioChange -> Json.Encode.Value
toJson change =
  case change of
    NewNote { decay, t, f } ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "note" )
        , ( "t", Json.Encode.float t )
        , ( "f", Json.Encode.float f )
        , ( "decay", Json.Encode.float decay )
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
