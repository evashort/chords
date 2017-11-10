module Main exposing (..)

import AudioChange
import AudioTime
import CachedChord exposing (CachedChord)
import Chord exposing (Chord)
import Highlight exposing (Highlight)
import MainParser
import Schedule exposing (Schedule, Segment)
import SelectionChange
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import TickTime

import AnimationFrame
import Html exposing
  (Html, Attribute, a, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style, spellcheck, id)
import Html.Events exposing (on, onInput)
import Json.Decode exposing (Decoder)
import Process
import Set exposing (Set)
import Task exposing (Task)
import Time

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
  , schedule : Schedule Chord
  , tick : Int
  , text : String
  , parse : MainParser.Model
  , suggestion : String
  , recentlyCopied : Set String
  }

init : ( Model, Cmd Msg )
init =
  ( { start = 0
    , schedule = { stop = 0, segments = [] }
    , tick = 0
    , text = defaultText
    , parse = MainParser.init (Substring 0 defaultText)
    , suggestion = ""
    , recentlyCopied = Set.empty
    }
  , Cmd.none
  )

defaultText : String
defaultText =
  "F   Csus4 C   G  G7\nDm7 FM7   _   E  E7\nDm  Asus4 Am  Em\nB0\n"

-- UPDATE

type Msg
  = NeedsTime (Float -> Msg)
  | CurrentTime Float
  | PlayChord ( Chord, Float )
  | TextEdited String
  | ShowSuggestion Suggestion
  | HideSuggestion Suggestion
  | SuggestionCopied Suggestion
  | RemoveCopied String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime partialMsg ->
      ( model, Task.perform partialMsg AudioTime.now )

    CurrentTime now ->
      ( if TickTime.nextBeat model.start now > model.schedule.stop then
          { model
          | start = 0
          , schedule = { stop = 0, segments = [] }
          , tick = 0
          }
        else
          { model | tick = TickTime.toTick model.start now }
      , Cmd.none
      )

    PlayChord ( chord, now ) ->
      let
        wouldBeat = TickTime.nextBeat model.start now
      in let
        ( start, beat, schedule, highStart, mute ) =
          case Schedule.get (wouldBeat - 1) model.schedule of
            Nothing ->
              ( now, 0, { stop = 0, segments = [] }, False, True )
            Just segment ->
              ( model.start
              , wouldBeat
              , Schedule.dropBefore (wouldBeat - 9) model.schedule
              , segment.start == wouldBeat - 8 && segment.x /= chord
              , segment.x /= chord
              )
      in let
        arpeggio =
          if highStart then
            [ 0, 2 * List.length chord ] :: arpeggioTail
          else
            [ 0 ] :: arpeggioTail
      in let
        stop = beat + 16
      in
        ( { model
          | start = start
          , schedule =
              Schedule.add stop { x = chord, start = beat } schedule
          }
        , AudioChange.playNotes
            mute
            now
            (List.map (TickTime.get start) (List.range beat (stop - 1)))
            (List.map (List.map (Chord.get chord)) arpeggio)
        )

    TextEdited newText ->
      ( { model
        | text = newText
        , parse =
            MainParser.update (Substring 0 newText) model.parse
        }
      , Cmd.none
      )

    ShowSuggestion suggestion ->
      ( { model | suggestion = suggestion.s }, Cmd.none)

    HideSuggestion _ ->
      ( { model | suggestion = "" }, Cmd.none)

    SuggestionCopied suggestion ->
      ( { model
        | recentlyCopied = Set.insert suggestion.s model.recentlyCopied
        }
      , Cmd.batch
          [ SelectionChange.changeSelection suggestion.firstRange
          , Task.perform
              (always (RemoveCopied suggestion.s))
              (Process.sleep (1 * Time.second))
          ]
      )

    RemoveCopied s ->
      ( { model | recentlyCopied = Set.remove s model.recentlyCopied }
      , Cmd.none
      )

halfArpeggioTail : List (List Int)
halfArpeggioTail = [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ] ]

arpeggioTail : List (List Int)
arpeggioTail = halfArpeggioTail ++ [ 0, 6 ] :: halfArpeggioTail

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.schedule.segments of
    [] -> Sub.none
    _ -> AnimationFrame.times (always (NeedsTime CurrentTime))

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family"
          , "\"Lucida Sans Unicode\", \"Lucida Grande\", sans-serif"
          )
        , ( "font-size", "10pt" )
        ]
    ]
    [ div
        [ style
            [ ( "width", "500px" )
            , ( "position", "relative" )
            , ( "font-size", "20pt" )
            , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
            ]
        ]
        [ textarea
            [ onInput TextEdited
            , spellcheck False
            , id "chordBox"
            , style
                [ ( "font", "inherit" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "padding", "10px" )
                , ( "border", "2px inset #e3e3e3")
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
                , ( "border", "2px solid transparent")
                , ( "margin", "0px" )
                , ( "white-space", "pre-wrap" )
                , ( "word-wrap", "break-word" )
                , ( "color", "transparent" )
                ]
            ]
            ( Highlight.view
                (model.text ++ "\n")
                (MainParser.view model.suggestion model.parse)
            )
        ]
    , div
        [ style
            [ ( "margin-top", "3px" ) ]
        ]
        ( List.map
            ( Suggestion.view
                ShowSuggestion
                HideSuggestion
                SuggestionCopied
                model.recentlyCopied
            )
            (MainParser.getSuggestions model.parse)
        )
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
              (Maybe.map .x (Schedule.get model.tick model.schedule))
              (Maybe.map .x (Schedule.next model.tick model.schedule))
          )
          (MainParser.getChords model.parse)
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewLine : Maybe Chord -> Maybe Chord -> List (Maybe CachedChord) -> Html Msg
viewLine activeChord nextChord line =
  div
    [ style
        [ ( "display", "flex" ) ]
    ]
    (List.map (viewMaybeChord activeChord nextChord) line)

viewMaybeChord : Maybe Chord -> Maybe Chord -> Maybe CachedChord -> Html Msg
viewMaybeChord activeChord nextChord maybeChord =
  case maybeChord of
    Just chord ->
      viewChord activeChord nextChord chord
    Nothing ->
      viewSpace

viewChord : Maybe Chord -> Maybe Chord -> CachedChord -> Html Msg
viewChord activeChord nextChord chord =
  let
    selected =
      activeChord == Just chord.chord || nextChord == Just chord.chord
  in
    span
      [ style
          [ ( "border-style"
            , if nextChord == Just chord.chord then
                "dashed"
              else
                "solid"
            )
          , ( "width", "75px" )
          , ( "flex", "none" )
          , ( "border-width", "5px" )
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
      [ div
          [ onLeftDown (NeedsTime (PlayChord << (,) chord.chord))
          , style
              [ ( "background", CachedChord.bg chord )
              , ( "color", CachedChord.fg chord )
              , ( "height", "75px" )
              , ( "display", "flex" )
              , ( "align-items", "center" )
              , ( "justify-content", "center" )
              , ( "border-radius", "5px" )
              , ( "box-shadow", "1px 1px 3px rgba(0, 0, 0, 0.6)" )
              , ( "cursor", "pointer" )
              , ( "white-space", "nowrap" )
              ]
          ]
          [ div [] (CachedChord.view chord) ]
      ]

onLeftDown : msg -> Attribute msg
onLeftDown message =
  on
    "mousedown"
    ( Json.Decode.andThen
        (requireLeftButton message)
        (Json.Decode.field "button" Json.Decode.int)
    )

requireLeftButton : msg -> Int -> Decoder msg
requireLeftButton message button =
  case button of
    0 -> Json.Decode.succeed message
    _ -> Json.Decode.fail ("ignoring button " ++ toString button)

viewSpace : Html msg
viewSpace =
  span
    [ style
        [ ( "width", "80px" )
        , ( "height", "80px" )
        , ( "flex", "none" )
        ]
    ]
    []
