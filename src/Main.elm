port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Encode

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { audioMsgs : List AudioMsg
  , chordStopsAt : Maybe Float
  }

init : ( Model, Cmd Msg )
init =
  ( { audioMsgs = []
    , chordStopsAt = Nothing
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = NeedsTime AudioMsg
  | CurrentTime Float
  | CheckpointReached Float

type AudioMsg = PlayChord

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime audioMsg ->
      ( { model | audioMsgs = audioMsg :: model.audioMsgs }, timeRequest () )

    CurrentTime t ->
      let
        new =
          List.foldr
            (batchUpdate <| audioUpdate t)
            { model = { model | audioMsgs = [] }
            , cmds = []
            , changeLists = []
            }
            model.audioMsgs
      in let
        cmds = List.reverse new.cmds
        changes = List.concat <| List.reverse new.changeLists
      in
        ( new.model
        , Cmd.batch <| (changeAudio changes) :: cmds
        )

    CheckpointReached t ->
      case model.chordStopsAt of
        Just cutoff ->
          if t >= cutoff then
            ( { model | chordStopsAt = Nothing }, Cmd.none )
          else
            ( model, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

batchUpdate :
  (AudioMsg -> Model -> AudioUpdateResult) ->
    AudioMsg -> BatchUpdateResult -> BatchUpdateResult
batchUpdate f msg old =
  let { model, cmd, changes } = f msg old.model in
    { model = model
    , cmds = cmd :: old.cmds
    , changeLists = changes :: old.changeLists
    }

type alias BatchUpdateResult =
  { model : Model
  , cmds : List (Cmd Msg)
  , changeLists : List (List AudioChange)
  }

audioUpdate : Float -> AudioMsg -> Model -> AudioUpdateResult
audioUpdate t msg model =
  case msg of
    PlayChord ->
      { model = { model | chordStopsAt = Just (t + 2.4) }
      , cmd = setCheckpoint (t + 2.4)
      , changes =
          CancelFutureNotes t ::
            List.map
              NewNote
              (offsetsToNotes t 48 <| majorArpeggio ++ majorArpeggio)
      }

type alias AudioUpdateResult =
  { model : Model
  , cmd : Cmd Msg
  , changes : List AudioChange
  }

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

-- root octave is midi notes 48 - 59 (C2 - B2)

majorArpeggio : List Int
majorArpeggio = [ 12, 4, 7, 0, 4, 7, 0, 4 ]

offsetsToNotes : Float -> Int -> List Int -> List Note
offsetsToNotes t root offsets =
  List.map2
    Note
    ( List.map
        ((+) t << (*) 0.15 << toFloat)
        (List.range 0 <| List.length offsets)
    )
    (List.map (mtof << (+) root) offsets)

-- SUBSCRIPTIONS

port timeRequest : () -> Cmd msg
port currentTime : (Float -> msg) -> Sub msg

changeAudio : List AudioChange -> Cmd msg
changeAudio = changeAudioUsingJson << List.map audioChangeToJson

port changeAudioUsingJson : List Json.Encode.Value -> Cmd msg

audioChangeToJson : AudioChange -> Json.Encode.Value
audioChangeToJson change =
  case change of
    NewNote note ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "note" )
        , ( "t", Json.Encode.float note.t )
        , ( "f", Json.Encode.float note.f )
        ]
    MuteAllNotes t ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "mute" )
        , ( "t", Json.Encode.float t )
        ]
    CancelFutureNotes t ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "cancel" )
        , ( "t", Json.Encode.float t )
        ]

type AudioChange
  = NewNote Note
  | MuteAllNotes Float
  | CancelFutureNotes Float

type alias Note =
  { t : Float
  , f : Float
  }

port setCheckpoint : Float -> Cmd msg
port checkpointReached : (Float -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ currentTime CurrentTime
    , checkpointReached CheckpointReached
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button
        [ onClick <| NeedsTime PlayChord
        , style
            ( case model.chordStopsAt of
                Just _ -> [ ( "color", "#ff0000" ) ]
                Nothing -> []
            )
        ]
        [ text "do it" ]
    ]
