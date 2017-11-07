module Main exposing (..)

import AudioChange
import AudioTime
import Chord exposing (Chord)
import Highlight exposing (Highlight)
import MainParser
import Schedule exposing (Schedule, ScheduledChord)
import Substring exposing (Substring)
import TickTime

import AnimationFrame
import Html exposing (Html, a, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style, spellcheck)
import Html.Events exposing (onMouseDown, on, targetValue)
import Json.Decode
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
  { start : Float
  , schedule : Schedule
  , tick : Int
  , stop : Int
  , text : String
  , parse : MainParser.Model
  }

init : ( Model, Cmd Msg )
init =
  ( { start = 0
    , schedule = []
    , tick = 0
    , stop = 0
    , text = defaultText
    , parse = MainParser.init (Substring 0 defaultText)
    }
  , Cmd.none
  )

defaultText : String
defaultText =
  "F   Csus4 C   G  G7\nDm7 FM7   Am7 E  E7\nDm  Asus4 Am  Em\nB0\n"

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
      ( if TickTime.nextBeat model.start now > model.stop then
          { model | start = 0, schedule = [], tick = 0, stop = 0 }
        else
          { model | tick = TickTime.toTick model.start now }
      , Cmd.none
      )

    PlayChord ( chord, now ) ->
      let
        wouldBeat = TickTime.nextBeat model.start now
      in let
        lastChord = Schedule.get (wouldBeat - 1) model.schedule
      in let
        ( start, beat, schedule ) =
          case lastChord of
            Nothing ->
              ( now, 0, [] )
            Just _ ->
              ( model.start
              , wouldBeat
              , Schedule.dropBefore (wouldBeat - 9) model.schedule
              )
      in let
        arpeggioHead =
          if lastChord == Just chord then
            [ 0 ]
          else if beat == 8 || Schedule.isStop (beat - 8) schedule then
            [ 0, 2 * List.length chord ]
          else
            [ 0 ]
      in let
        stop = beat + 16
      in
        ( { model
          | start = start
          , schedule =
              Schedule.add beat { chord = chord, stop = stop } schedule
          , stop = stop
          }
        , AudioChange.playNotes
            (lastChord /= Just chord)
            now
            (List.map (TickTime.get start) (List.range beat (stop - 1)))
            (Chord.getSquared (arpeggioHead :: arpeggioTail) chord)
        )

    TextEdited newText ->
      ( { model
        | text = newText
        , parse =
            MainParser.update (Substring 0 newText) model.parse
        }
      , Cmd.none
      )

halfArpeggioTail : List (List Int)
halfArpeggioTail = [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ] ]

arpeggioTail : List (List Int)
arpeggioTail = halfArpeggioTail ++ [ 0, 6 ] :: halfArpeggioTail

-- SUBSCRIPTIONS

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
              (Schedule.get model.tick model.schedule)
              (Schedule.next model.tick model.schedule)
          )
          (MainParser.getChords model.parse)
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewLine : Maybe Chord -> Maybe Chord -> List Chord -> Html Msg
viewLine activeChord nextChord line =
  div [] (List.map (viewChord activeChord nextChord) line)

viewChord : Maybe Chord -> Maybe Chord -> Chord -> Html Msg
viewChord activeChord nextChord chord =
  let
    selected =
      activeChord == Just chord || nextChord == Just chord
  in
    span
      [ style
          [ ( "border-style"
            , if nextChord == Just chord then
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
