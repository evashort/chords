port module Main exposing (..)

import Arpeggio exposing (Arpeggio)
import AudioTime
import Chord exposing (Chord)
import Highlight exposing (Highlight)
import MainParser
import Metronome exposing (Metronome)
import Substring exposing (Substring)

import AnimationFrame
import Html exposing (Html, a, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style, spellcheck)
import Html.Events exposing (onMouseDown, on, targetValue)
import Json.Decode
import Json.Encode
import Task exposing (Task)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { metronome : Metronome
  , schedule : List ScheduledChord
  , text : String
  , parse : MainParser.Model
  }

type alias ScheduledChord =
  { chord : Chord
  , tick : Int
  }

init : ( Model, Cmd Msg )
init =
  let
    defaultChords =
      "F   Csus4 C   G  G7\nDm7 FM7   Am7 E  E7\nDm  Asus4 Am  Em\nB0\n"
  in
    ( { metronome = { start = 0, ticks = 0 }
      , schedule = []
      , text = defaultChords
      , parse = MainParser.init (Substring 0 defaultChords)
      }
    , Cmd.none
    )

-- UPDATE

type Msg
  = NeedsTime (Float -> Msg)
  | CurrentTime Float
  | PlayChord ( Chord, Float )
  | TextEdited String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime partialMsg ->
      ( model, Task.perform partialMsg AudioTime.now )

    CurrentTime now ->
      case model.schedule of
        [] ->
          ( model, Cmd.none )
        [ _ ] ->
          if now > Metronome.end model.metronome then
            ( { model | schedule = [] }, Cmd.none )
          else
            ( model, Cmd.none )
        x :: y :: rest ->
          if now >= Metronome.tickTime model.metronome y.tick then
            ( { model
              | schedule =
                  if x.chord == y.chord then
                    x :: rest
                  else
                    y :: rest
              }
            , Cmd.none
            )
          else
            ( model, Cmd.none )

    PlayChord ( chord, now ) ->
      let
        oldMetronome =
          case model.schedule of
            [] -> { start = now, ticks = 0 }
            _ -> model.metronome
      in let
        changeTick = Metronome.nextBeat oldMetronome now
      in let
        measureStart = changeTick - (changeTick % ticksPerMeasure)
      in let
        lastChangeTick =
          case model.schedule of
            x :: _ -> x.tick
            _ -> measureStart
      in let
        arpeggio = Chord.arpeggio lastChangeTick measureStart chord
      in let
        metronome =
          { oldMetronome
          | ticks =
              if measureStart + ticksPerMeasure - changeTick < minTicks then
                measureStart + 2 * ticksPerMeasure
              else
                measureStart + ticksPerMeasure
          }
      in let
        changeTime = Metronome.tickTime metronome changeTick
      in let
        audioChanges =
          ( if changeTick < oldMetronome.ticks then
              if now + latency >= changeTime then
                let notBeforeNote = max now changeTime in
                  [ MuteAllNotes
                      { t = notBeforeNote, before = False }
                  ]
              else
                [ MuteAllNotes { t = changeTime, before = True } ]
            else
              []
          ) ++
            ( List.concatMap
                (arpeggioNotes metronome.start now arpeggio)
                (List.range changeTick (metronome.ticks - 1))
            )
      in
        ( { model
          | metronome = metronome
          , schedule =
              case model.schedule of
                [] ->
                  [ { chord = chord, tick = changeTick } ]
                x :: _ ->
                  if changeTime > now then
                    [ x, { chord = chord, tick = changeTick } ]
                  else
                    [ { chord = chord, tick = changeTick } ]
          }
        , changeAudioUsingJson (List.map audioChangeToJson audioChanges)
        )

    TextEdited newText ->
      ( { model
        | text = newText
        , parse =
            MainParser.update (Substring 0 newText) model.parse
        }
      , Cmd.none
      )

ticksPerMeasure : Int
ticksPerMeasure = 8

minTicks : Int
minTicks = 9

latency : Float
latency = 0.01

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

arpeggioNotes : Float -> Float -> Arpeggio -> Int -> List AudioChange
arpeggioNotes start now arpeggio i =
  let t = max now (start + Metronome.interval * toFloat i) in
    List.map
      (NewNote << Note t << mtof)
      (Arpeggio.get i arpeggio)

-- SUBSCRIPTIONS

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

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.schedule of
    [] -> Sub.none
    _ -> AnimationFrame.times (always (NeedsTime CurrentTime))

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family", "calibri, helvetica, arial, sans-serif" )
        ]
    ]
    [ div
        [ style
            [ ( "width", "500px" )
            , ( "position", "relative" )
            , ( "border-style", "inset" )
            , ( "border-width", "2px" )
            , ( "border-color", "#e3e3e3")
            , ( "font-size", "20pt" )
            , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
            ]
        ]
        [ textarea
            [ on "input" (Json.Decode.map TextEdited targetValue)
            , spellcheck False
            , style
                [ ( "font", "inherit" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "padding", "10px" )
                , ( "border", "none" )
                , ( "margin", "0px" )
                , ( "position", "absolute" )
                , ( "resize", "none" )
                , ( "overflow", "hidden" )
                , ( "box-sizing", "border-box" )
                , ( "background", "transparent" )
                ]
            ]
            [ text model.text
            ]
        , pre
            [ style
                [ ( "font", "inherit" )
                , ( "padding", "10px" )
                , ( "margin", "0px" )
                , ( "white-space", "pre-wrap" )
                , ( "word-wrap", "break-word" )
                , ( "color", "transparent" )
                ]
            ]
            ( Highlight.view
                (model.text ++ "\n")
                (MainParser.view model.parse)
            )
        ]
    , div
        [ style
            [ ( "min-height", "200px" )
            , ( "font-size", "20pt" )
            , ( "margin-right", "5px" )
            , ( "margin-bottom", "55px" )
            ]
        ] <|
        List.map
          ( viewLine
            ( case model.schedule of
                [] -> []
                x :: _ -> x.chord
            )
            ( case model.schedule of
                _ :: y :: _ -> y.chord
                _ -> []
            )
          )
          (MainParser.getChords model.parse)
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewLine : Chord -> Chord -> List Chord -> Html Msg
viewLine activeChord nextChord line =
  div [] (List.map (viewChord activeChord nextChord) line)

viewChord : Chord -> Chord -> Chord -> Html Msg
viewChord activeChord nextChord chord =
  let
    selected =
      chord == activeChord || chord == nextChord
  in
    span
      [ style
          [ ( "border-style"
            , if chord == nextChord then
                "dashed"
              else
                "solid"
            )
          , ( "border-width", "5px" )
          , ( "display", "inline-block" )
          , ( "margin-right", "-5px" )
          , ( "margin-bottom", "-5px" )
          , ( "border-color"
            , if selected then
                "#3399ff"
              else
                "transparent"
            )
          , ( "border-radius", "10px" )
          ]
      ]
      [ span
          [ onMouseDown <| NeedsTime (PlayChord << (,) chord)
          , style
              [ ( "background", Chord.bg chord )
              , ( "color", Chord.fg chord )
              , ( "width", "75px" )
              , ( "line-height", "75px" )
              , ( "text-align", "center" )
              , ( "display", "inline-block" )
              , ( "border-radius", "5px" )
              , ( "box-shadow", "1px 1px 3px rgba(0, 0, 0, 0.6)" )
              , ( "cursor", "pointer" )
              , ( "white-space", "nowrap" )
              ]
          ]
          (Chord.view chord)
      ]
