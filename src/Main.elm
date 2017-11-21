port module Main exposing (..)

import AudioChange
import AudioTime
import CachedChord
import Chord exposing (Chord)
import ChordParser exposing (IdChord)
import Highlight exposing (Highlight)
import MainParser
import Schedule exposing (Schedule, Segment)
import Selection
import Substring exposing (Substring)
import SuggestionBar
import TickTime

import AnimationFrame
import Html exposing
  (Html, Attribute, a, button, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style, spellcheck, id)
import Html.Events exposing (on, onInput, onFocus, onBlur, onClick)
import Html.Lazy
import Json.Decode exposing (Decoder)
import Navigation exposing (Location)
import Task exposing (Task)
import Time
import Url

main =
  Navigation.programWithFlags
    UrlChange
    { init = init
    , view = Html.Lazy.lazy view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { start : Float
  , schedule : Schedule Int
  , tick : Int
  , text : String
  , parse : MainParser.Model
  , home : Bool
  , subscribeToSelection : Bool
  , chordBoxFocused : Bool
  , bubble : Maybe Highlight
  , chordBox : ChordBox
  , suggestionBar : SuggestionBar.Model
  , chordArea : ChordArea
  }

type alias ChordBox =
  { highlightRanges : List Substring
  , bubble : Maybe Highlight
  }

type alias ChordArea =
  { activeChord : Int
  , nextChord : Int
  }

init : Bool -> Location -> ( Model, Cmd Msg )
init mac location =
  let
    text = textFromLocation location
  in let
    parse = MainParser.init (Substring 0 text)
  in let
    suggestions = MainParser.getSuggestions parse
  in let
    n = String.length text
  in let
    modifierKey = if mac then "⌘" else "Ctrl+"
  in let
    suggestionBar = SuggestionBar.init modifierKey suggestions
  in
    ( { start = 0
      , schedule = { stop = 0, segments = [] }
      , tick = 0
      , text = text
      , parse = parse
      , home = True
      , subscribeToSelection = True
      , chordBoxFocused = True
      , bubble = Nothing
      , chordBox =
          { highlightRanges =
              SuggestionBar.highlightRanges suggestionBar
          , bubble = Nothing
          }
      , suggestionBar = suggestionBar
      , chordArea =
          { activeChord = -1
          , nextChord = -1
          }
      }
    , Selection.setSelection { start = n, stop = n }
    )

textFromLocation : Location -> String
textFromLocation location =
  Maybe.withDefault defaultText (Url.hashParamValue "text" location)

defaultText : String
defaultText =
  "F   Csus4 C   G  G7\nDm7 FM7   _   E  E7\nDm  Asus4 Am  Em\nB0\n"

-- UPDATE

type Msg
  = NeedsTime (Float -> Msg)
  | CurrentTime Float
  | PlayChord ( Chord, Int, Float )
  | TextEdited String
  | UrlChange Location
  | CheckSelection
  | ReceivedSelection Selection.Model
  | ChordBoxFocused Bool
  | SuggestionBarMsg SuggestionBar.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime partialMsg ->
      ( model, Task.perform partialMsg AudioTime.now )

    CurrentTime now ->
      ( if TickTime.nextBeat model.start now > model.schedule.stop then
          let schedule = { stop = 0, segments = [] } in
            { model
            | start = 0
            , schedule = schedule
            , tick = 0
            , chordArea = updateChordArea model.chordArea schedule 0
            }
        else
          let tick = TickTime.toTick model.start now in
            if tick /= model.tick then
              { model
              | tick = TickTime.toTick model.start now
              , chordArea =
                  updateChordArea model.chordArea model.schedule tick
              }
            else
              model
      , Cmd.none
      )

    PlayChord ( chord, id, now ) ->
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
              , segment.start == wouldBeat - 8 && segment.x /= id
              , segment.x /= id
              )
      in let
        arpeggio =
          if highStart then
            [ 0, 2 * List.length chord ] :: arpeggioTail
          else
            [ 0 ] :: arpeggioTail
      in let
        stop = beat + 16
      in let
        newSchedule =
          Schedule.add stop { x = id, start = beat } schedule
      in
        ( { model
          | start = start
          , schedule = newSchedule
          , chordArea =
              updateChordArea model.chordArea newSchedule model.tick
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
      in
        updateSuggestionBar
          (SuggestionBar.SuggestionsChanged suggestions)
          { model
          | text = newText
          , parse = parse
          , home = False
          }
          [ if model.home then
              Navigation.newUrl
                ("#text=" ++ Url.percentEncode newText)
            else
              Navigation.modifyUrl
                ("#text=" ++ Url.percentEncode newText)
          ]

    UrlChange location ->
      let newText = textFromLocation location in
        if newText /= model.text then
          let
            parse = MainParser.update (Substring 0 newText) model.parse
          in let
            suggestions = MainParser.getSuggestions parse
          in
            updateSuggestionBar
              (SuggestionBar.SuggestionsChanged suggestions)
              { model
              | text = newText
              , parse = parse
              , home = True
              }
              [ setChordBoxText newText ]
        else
          ( model, Cmd.none )

    CheckSelection ->
      ( model, Selection.checkSelection () )

    ReceivedSelection { landingPad, selection } ->
      let
        bubble =
          case landingPad of
            Nothing ->
              Nothing
            Just lp ->
              if selection == lp.selection then
                Just
                  ( Highlight
                      ( model.suggestionBar.modifierKey ++
                          if
                            lp.selection.start == lp.selection.stop
                          then
                            "V to paste here"
                          else
                            "V to replace"
                      )
                      ""
                      ""
                      (Substring lp.selection.start "")
                  )
              else
                Nothing
      in
        updateSuggestionBar
          ( SuggestionBar.LandingPadSelected
              ( case landingPad of
                  Nothing ->
                    False
                  Just lp ->
                    lp.source == "suggestion" &&
                      selection == lp.selection
              )
          )
          { model
          | bubble = bubble
          , chordBox =
              updateChordBoxBubble
                bubble
                model.chordBoxFocused
                model.chordBox
          }
          []

    ChordBoxFocused chordBoxFocused ->
      updateSuggestionBar
        (SuggestionBar.ChordBoxFocused chordBoxFocused)
        { model
        | chordBoxFocused = chordBoxFocused
        , subscribeToSelection =
            model.subscribeToSelection || chordBoxFocused
        , chordBox =
            updateChordBoxBubble
              model.bubble
              chordBoxFocused
              model.chordBox
        }
        []

    SuggestionBarMsg msg ->
      updateSuggestionBar msg model []

halfArpeggioTail : List (List Int)
halfArpeggioTail = [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ] ]

arpeggioTail : List (List Int)
arpeggioTail = halfArpeggioTail ++ [ 0, 6 ] :: halfArpeggioTail

updateChordBoxBubble : Maybe Highlight -> Bool -> ChordBox -> ChordBox
updateChordBoxBubble bubble chordBoxFocused chordBox =
  let chordBoxBubble = if chordBoxFocused then bubble else Nothing in
    if chordBoxBubble /= chordBox.bubble then
      { chordBox | bubble = chordBoxBubble }
    else
      chordBox

updateSuggestionBar :
  SuggestionBar.Msg -> Model -> List (Cmd Msg) -> ( Model, Cmd Msg )
updateSuggestionBar msg model cmds =
  let
    ( suggestionBar, suggestionBarCmd ) =
      SuggestionBar.update msg model.suggestionBar
  in let
    highlightRanges = SuggestionBar.highlightRanges suggestionBar
  in let
    chordBox = model.chordBox
  in
    ( { model
      | chordBox =
          if highlightRanges /= chordBox.highlightRanges then
            { chordBox | highlightRanges = highlightRanges }
          else
            chordBox
      , suggestionBar = suggestionBar
      }
    , if List.isEmpty cmds then
        Cmd.map SuggestionBarMsg suggestionBarCmd
      else
        Cmd.batch (Cmd.map SuggestionBarMsg suggestionBarCmd :: cmds)
    )

updateChordArea : ChordArea -> Schedule Int -> Int -> ChordArea
updateChordArea chordArea schedule tick =
  let
    activeChord =
      case Schedule.get tick schedule of
        Just segment -> segment.x
        Nothing -> -1
  in let
    nextChord = case Schedule.next tick schedule of
      Just segment -> segment.x
      Nothing -> -1
  in
    if
      activeChord /= chordArea.activeChord ||
        nextChord /= chordArea.nextChord
    then
      { activeChord = activeChord
      , nextChord = nextChord
      }
    else
      chordArea

-- SUBSCRIPTIONS

port setChordBoxText : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    ( List.filterMap
        identity
        [ Just (Selection.receiveModel ReceivedSelection)
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
    [ Html.Lazy.lazy3 viewChordBox model.text model.parse model.chordBox
    , Html.Lazy.lazy viewSuggestionBar model.suggestionBar
    , Html.Lazy.lazy2 viewChordArea model.chordArea model.parse
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewChordBox : String -> MainParser.Model -> ChordBox -> Html Msg
viewChordBox chordBoxText parse chordBox =
  div
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
        , onLeftClick CheckSelection
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
        [ text chordBoxText ]
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
        ( List.map
            Highlight.view
            (Highlight.mergeLayers (getLayers chordBoxText parse chordBox))
        )
    ]

getLayers : String -> MainParser.Model -> ChordBox -> List (List Highlight)
getLayers chordBoxText parse chordBox =
  [ let
      grays =
        List.map
          (Highlight "" "#ffffff" "#aaaaaa")
          chordBox.highlightRanges
    in
      case chordBox.bubble of
        Just bubble -> bubble :: grays
        Nothing -> grays
  , MainParser.view parse
  , [ Highlight
        ""
        "#000000"
        "#ffffff"
        (Substring 0 (chordBoxText ++ "\n"))
    ]
  ]

viewSuggestionBar : SuggestionBar.Model -> Html Msg
viewSuggestionBar = Html.map SuggestionBarMsg << SuggestionBar.view

viewChordArea : ChordArea -> MainParser.Model -> Html Msg
viewChordArea chordArea parse =
  div
    [ style
        [ ( "min-height", "200px" )
        , ( "font-size", "18pt" )
        , ( "margin-right", "5px" )
        , ( "margin-bottom", "55px" )
        ]
    ]
    (List.map (viewLine chordArea) (MainParser.getChords parse))

viewLine : ChordArea -> List (Maybe IdChord) -> Html Msg
viewLine chordArea line =
  div
    [ style
        [ ( "display", "flex" ) ]
    ]
    (List.map (viewMaybeChord chordArea) line)

viewMaybeChord : ChordArea -> Maybe IdChord -> Html Msg
viewMaybeChord chordArea maybeChord =
  case maybeChord of
    Just chord ->
      viewChord chordArea chord
    Nothing ->
      viewSpace

viewChord : ChordArea -> IdChord -> Html Msg
viewChord chordArea chord =
  let
    selected =
      chordArea.activeChord == chord.id ||
        chordArea.nextChord == chord.id
  in
    span
      [ style
          [ ( "border-style"
            , if chordArea.nextChord == chord.id then
                "dashed"
              else
                "solid"
            )
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
      [ button
          [ onLeftDown
              (NeedsTime (PlayChord << (,,) chord.cache.chord chord.id))
          , onSpaceOrEnterDown
              (NeedsTime (PlayChord << (,,) chord.cache.chord chord.id))
          , style
              [ ( "background", CachedChord.bg chord.cache )
              , ( "color", CachedChord.fg chord.cache )
              , ( "font", "inherit" )
              , ( "width", "75px" )
              , ( "height", "75px" )
              , ( "padding", "0px 3px" )
              , ( "border"
                , "1px solid rgba(0, 0, 0, " ++
                    toString
                      ( if CachedChord.fg chord.cache == "#ffffff" then
                          0.8
                        else
                          0.3
                      ) ++ ")"
                )
              , ( "border-radius", "5px" )
              , ( "box-shadow"
                , "inset 18px 34px 20px -20px rgba(255, 255, 255, " ++
                    toString
                      ( if CachedChord.fg chord.cache == "#ffffff" then
                          0.6
                        else
                          0.7
                      ) ++ ")"
                )
              , ( "cursor", "pointer" )
              , ( "white-space", "nowrap" )
              ]
          ]
          (CachedChord.view chord.cache)
      ]

onLeftDown : msg -> Attribute msg
onLeftDown message =
  on
    "mousedown"
    ( Json.Decode.andThen
        (requireLeftButton message)
        (Json.Decode.field "button" Json.Decode.int)
    )

onLeftClick : msg -> Attribute msg
onLeftClick message =
  on
    "click"
    ( Json.Decode.andThen
        (requireLeftButton message)
        (Json.Decode.field "button" Json.Decode.int)
    )

requireLeftButton : msg -> Int -> Decoder msg
requireLeftButton message button =
  case button of
    0 -> Json.Decode.succeed message
    _ -> Json.Decode.fail ("ignoring button " ++ toString button)

onSpaceOrEnterDown : msg -> Attribute msg
onSpaceOrEnterDown message =
  on
    "keydown"
    ( Json.Decode.andThen
        (requireSpaceOrEnter message)
        (Json.Decode.field "which" Json.Decode.int)
    )

requireSpaceOrEnter : msg -> Int -> Decoder msg
requireSpaceOrEnter message which =
  case which of
    13 -> Json.Decode.succeed message
    32 -> Json.Decode.succeed message
    _ -> Json.Decode.fail ("ignoring key " ++ toString which)

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
