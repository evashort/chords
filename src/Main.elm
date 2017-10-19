port module Main exposing (..)

import Metronome exposing (Metronome)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown)
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
  , playing : Maybe PlayInfo
  }

type alias PlayInfo =
  { metronome : Metronome
  , chordIndex : Int
  }

init : ( Model, Cmd Msg )
init =
  ( { audioMsgs = []
    , playing = Nothing
    }
  , Cmd.none
  )

type alias Chord =
  { name : String
  , root : Int
  , arpeggio : List Int
  }

errorChord : Chord
errorChord =
  Chord "error" 72 [ 0 ]

chords : List Chord
chords =
  [ Chord "C" 48 majorArpeggio
  , Chord "d" 50 minorArpeggio
  , Chord "e" 52 minorArpeggio
  , Chord "F" 53 majorArpeggio
  , Chord "G" 55 majorArpeggio
  , Chord "a" 57 minorArpeggio
  , Chord "b°" 59 diminishedArpeggio
  ]

majorArpeggio : List Int
majorArpeggio = [ 12, 4, 7, 0, 4, 7, 0, 4 ]

minorArpeggio : List Int
minorArpeggio = [ 12, 3, 7, 0, 3, 7, 0, 3 ]

diminishedArpeggio : List Int
diminishedArpeggio = [ 12, 3, 6, 0, 3, 6, 0, 3 ]

-- UPDATE

type Msg
  = NeedsTime AudioMsg
  | CurrentTime Float
  | CheckpointReached Float

type AudioMsg = PlayChord Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime audioMsg ->
      ( { model | audioMsgs = audioMsg :: model.audioMsgs }, timeRequest () )

    CurrentTime now ->
      let
        new =
          List.foldr
            (batchUpdate <| audioUpdate now)
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

    CheckpointReached now ->
      case model.playing of
        Just p ->
          if now >= Metronome.getStop p.metronome then
            ( { model | playing = Nothing }, Cmd.none )
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
audioUpdate now msg model =
  case msg of
    PlayChord chordIndex ->
      let
        start =
          case model.playing of
            Nothing -> now
            Just p -> p.metronome.start
      in let
        oldTicks =
          case model.playing of
            Nothing -> 0
            Just p -> p.metronome.ticks
      in let
        changeTick =
          case model.playing of
            Nothing -> 0
            Just p -> Metronome.getNextBeat p.metronome now
      in let
        chord =
          Maybe.withDefault errorChord <|
            List.head (List.drop chordIndex chords)
      in let
        arpeggioStartIndex = changeTick % List.length chord.arpeggio
      in let
        missingTicks =
          max 0 <|
            minTicks - (List.length chord.arpeggio - arpeggioStartIndex)
      in let
        extraCopies =
          (missingTicks + List.length chord.arpeggio - 1) //
            List.length chord.arpeggio
      in let
        offsets =
          List.concat <|
            (List.drop arpeggioStartIndex chord.arpeggio) ::
              List.repeat extraCopies chord.arpeggio
      in let
        m = { start = start, ticks = changeTick + List.length offsets }
      in let
        changeTime = Metronome.getTickTime m changeTick
      in
        { model =
          { model
          | playing = Just { metronome = m, chordIndex = chordIndex }
          }
        , cmd =
            setCheckpoint (Metronome.getStop m)
        , changes =
              ( if changeTick < oldTicks then
                  if now + latency >= changeTime then
                    let notBeforeNote = max now changeTime in
                      [ CancelFutureNotes
                          { t = notBeforeNote, before = False }
                      , MuteLoudestNote notBeforeNote
                      ]
                  else
                    [ CancelFutureNotes { t = changeTime, before = True } ]
                else
                  []
              ) ++
                ( offsetsToNotes
                    start
                    changeTick
                    now
                    chord.root
                    offsets
                )
        }

type alias AudioUpdateResult =
  { model : Model
  , cmd : Cmd Msg
  , changes : List AudioChange
  }

minTicks : Int
minTicks = 9

latency : Float
latency = 0.01

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

-- root octave is midi notes 48 - 59 (C2 - B2)

offsetsToNotes : Float -> Int -> Float -> Int -> List Int -> List AudioChange
offsetsToNotes start changeTick now root offsets =
  List.map2
    (toNote now)
    ( List.map
        ((+) start << (*) Metronome.interval << toFloat)
        (List.range changeTick <| changeTick + List.length offsets)
    )
    (List.map (mtof << (+) root) offsets)

toNote : Float -> Float -> Float -> AudioChange
toNote now t f =
  NewNote (Note (max now t) f)

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
    MuteLoudestNote t ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "muteLoudest" )
        , ( "t", Json.Encode.float t )
        ]
    MuteAllNotes ct ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "mute" )
        , ( "t", Json.Encode.float ct.t )
        , ( "before", Json.Encode.bool ct.before )
        ]
    CancelFutureNotes ct ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "cancel" )
        , ( "t", Json.Encode.float ct.t )
        , ( "before", Json.Encode.bool ct.before )
        ]

type AudioChange
  = NewNote Note
  | MuteLoudestNote Float
  | MuteAllNotes ChangeTime
  | CancelFutureNotes ChangeTime

type alias Note =
  { t : Float
  , f : Float
  }

type alias ChangeTime =
  { t : Float
  , before : Bool
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
  div [] <|
    List.indexedMap
      ( viewChord
        ( case model.playing of
            Nothing -> -1
            Just p -> p.chordIndex
        )
      )
      chords

viewChord : Int -> Int -> Chord -> Html Msg
viewChord activeChordIndex chordIndex chord =
  button
    [ onMouseDown <| NeedsTime (PlayChord chordIndex)
    , style
        ( if chordIndex == activeChordIndex then
            [ ( "color", "#ff0000" ) ]
          else
            []
        )
    ]
    [ text chord.name ]
