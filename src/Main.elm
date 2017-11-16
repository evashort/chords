module Main exposing (..)

import AudioChange
import AudioTime
import CachedChord exposing (CachedChord)
import Chord exposing (Chord)
import Highlight exposing (Highlight)
import MainParser
import Schedule exposing (Schedule, Segment)
import Selection
import Substring exposing (Substring)
import SuggestionBar
import TickTime

import AnimationFrame
import Html exposing
  (Html, Attribute, a, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style, spellcheck, id)
import Html.Events exposing (on, onInput, onFocus, onBlur)
import Json.Decode exposing (Decoder)
import Task exposing (Task)
import Time

main =
  Html.programWithFlags
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
  , selection : ( Int, Int )
  , subscribeToSelection : Bool
  , chordBoxFocused : Bool
  , suggestionBar : SuggestionBar.Model
  }

init : Bool -> ( Model, Cmd Msg )
init mac =
  let n = String.length defaultText in
    ( { start = 0
      , schedule = { stop = 0, segments = [] }
      , tick = 0
      , text = defaultText
      , parse = MainParser.init (Substring 0 defaultText)
      , selection = ( n, n )
      , subscribeToSelection = True
      , chordBoxFocused = True
      , suggestionBar = SuggestionBar.init mac
      }
    , Selection.setSelection ( n, n )
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
  | CheckSelection
  | ReceivedSelection ( Int, Int )
  | ChordBoxFocused Bool
  | SuggestionBarMsg SuggestionBar.Msg

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
      let
        parse = MainParser.update (Substring 0 newText) model.parse
      in let
        suggestions = MainParser.getSuggestions parse
      in let
        ( suggestionBar, suggestionBarCmd ) =
          SuggestionBar.update
            (SuggestionBar.SuggestionsChanged suggestions)
            model.suggestionBar
      in
        ( { model
          | text = newText
          , parse = parse
          , suggestionBar = suggestionBar
          }
        , Cmd.map SuggestionBarMsg suggestionBarCmd
        )

    CheckSelection ->
      ( model, Selection.checkSelection () )

    ReceivedSelection selection ->
      let
        ( suggestionBar, suggestionBarCmd ) =
          SuggestionBar.update
            (SuggestionBar.ReceivedSelection selection)
            model.suggestionBar
      in
        ( { model
          | selection = selection
          , subscribeToSelection = model.chordBoxFocused
          , suggestionBar = suggestionBar
          }
        , Cmd.map SuggestionBarMsg suggestionBarCmd
        )

    ChordBoxFocused chordBoxFocused ->
      let
        ( suggestionBar, suggestionBarCmd ) =
          SuggestionBar.update
            (SuggestionBar.ChordBoxFocused chordBoxFocused)
            model.suggestionBar
      in
        ( { model
          | chordBoxFocused = chordBoxFocused
          , subscribeToSelection =
              model.subscribeToSelection || chordBoxFocused
          , suggestionBar = suggestionBar
          }
        , if chordBoxFocused then
            Cmd.batch
              [ Selection.checkSelection ()
              , Cmd.map SuggestionBarMsg suggestionBarCmd
              ]
          else
            Cmd.map SuggestionBarMsg suggestionBarCmd
        )

    SuggestionBarMsg msg ->
      let
        ( suggestionBar, suggestionBarCmd ) =
          SuggestionBar.update msg model.suggestionBar
      in
        ( { model | suggestionBar = suggestionBar }
        , Cmd.map SuggestionBarMsg suggestionBarCmd
        )

halfArpeggioTail : List (List Int)
halfArpeggioTail = [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ] ]

arpeggioTail : List (List Int)
arpeggioTail = halfArpeggioTail ++ [ 0, 6 ] :: halfArpeggioTail

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    ( List.filterMap
        identity
        [ Just (Selection.receiveSelection ReceivedSelection)
        , if model.subscribeToSelection then
            Just (Time.every (1 * Time.second) (always CheckSelection))
          else
            Nothing
        , if model.schedule.segments /= [] then
            Just (AnimationFrame.times (always (NeedsTime CurrentTime)))
          else
            Nothing
        ]
    )

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family", "Arial, Helvetica, sans-serif" )
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
            , onFocus (ChordBoxFocused True)
            , onBlur (ChordBoxFocused False)
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
            ( ( List.map Highlight.view <<
                  Highlight.mergeLayers <<
                    List.filterMap identity
              )
                [ case
                    ( model.suggestionBar.landingPad
                    , model.suggestionBar.landingPadSelected
                    , model.chordBoxFocused
                    )
                  of
                    ( Just landingPad, True, True ) ->
                      Just
                        [ Highlight
                            ( model.suggestionBar.modifierKey ++
                                "-V to replace"
                            )
                            "#ffffff"
                            "#ff0000"
                            (Substring landingPad.i "")
                        ]
                    _ ->
                      Nothing
                , case model.suggestionBar.highlighted of
                    Just suggestion ->
                      Just
                        ( List.map
                            (Highlight "" "#ffffff" "#aaaaaa")
                            (suggestion.firstRange :: suggestion.ranges)
                        )
                    Nothing ->
                      Nothing
                , Just (MainParser.view model.parse)
                , Just
                    [ Highlight
                        ""
                        "#000000"
                        "#ffffff"
                        (Substring 0 (model.text ++ "\n"))
                    ]
                ]
            )
        ]
    , Html.map SuggestionBarMsg (SuggestionBar.view model.suggestionBar)
    , div
        [ style
            [ ( "min-height", "200px" )
            , ( "font-size", "18pt" )
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
