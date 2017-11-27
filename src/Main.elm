port module Main exposing (..)

import AudioChange
import AudioTime
import CachedChord
import Chord exposing (Chord)
import ChordParser exposing (IdChord)
import CircleOfFifths
import CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)
import Highlight exposing (Highlight)
import MainParser
import Schedule exposing (Schedule, Segment)
import Selection
import Substring exposing (Substring)
import SuggestionBar
import TickTime

import AnimationFrame
import Html exposing (Html, a, button, div, pre, span, text, textarea, input)
import Html.Attributes exposing
  (href, style, spellcheck, id, classList, type_)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Html.Lazy
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
  , strum : Bool
  , strumInterval : Float
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
    parse =
      MainParser.init CircleOfFifths.chordCount (Substring 0 text)
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
      , strum = False
      , strumInterval = 0
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
  | SetStrum ( Bool, Float )
  | SetStrumInterval String
  | FocusHorizontal ( Bool, Int )
  | FocusVertical ( Bool, Int )
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
      ( if
          not model.strum &&
            TickTime.nextBeat model.start now > model.schedule.stop
        then
          let schedule = { stop = 0, segments = [] } in
            { model
            | start = 0
            , schedule = schedule
            , tick = 0
            , chordArea = updateChordArea model.chordArea schedule 0
            }
        else
          let tick = TickTime.toTick model.start now in
            if model.strum && tick >= model.schedule.stop then
              let schedule = { stop = 0, segments = [] } in
                { model
                | start = 0
                , schedule = schedule
                , tick = 0
                , chordArea = updateChordArea model.chordArea schedule 0
                }
            else if tick /= model.tick then
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
        ( start, schedule, cmd ) =
          if model.strum then
            playStrum
              model.strumInterval chord id now model.start model.schedule
          else
            playArpeggio chord id now model.start model.schedule
      in let
        tick = TickTime.toTick start now
      in
        ( { model
          | start = start
          , schedule = schedule
          , tick = tick
          , chordArea =
              updateChordArea model.chordArea schedule tick
          }
        , cmd
        )

    SetStrum ( strum, now ) ->
      if strum == model.strum then
        ( model, Cmd.none)
      else
        ( let schedule = { stop = 0, segments = [] } in
            { model
            | start = 0
            , schedule = schedule
            , tick = 0
            , strum = strum
            , chordArea = updateChordArea model.chordArea schedule 0
            }
        , AudioChange.muteAllNotes now
        )

    SetStrumInterval strumIntervalString ->
      ( case String.toFloat strumIntervalString of
          Ok strumInterval ->
            { model | strumInterval = strumInterval }
          Err _ ->
            model
      , Cmd.none
      )

    FocusHorizontal ( forwards, id ) ->
      case
        notBeforeJust
          ( List.tail <<
              notBeforeTrue ((==) id << .id) <<
                (if forwards then identity else List.reverse) <<
                  List.filterMap identity
          )
          (MainParser.getChords model.parse)
      of
        Just ( chord :: _, _ ) ->
          ( model, focusById (toString chord.id) )
        _ ->
          ( model, Cmd.none )

    FocusVertical ( forwards, id ) ->
      case
        notBeforeJust
          (findTrue ((==) (Just id) << Maybe.map .id))
          ( (if forwards then identity else List.reverse)
              (MainParser.getChords model.parse)
          )
      of
        Nothing ->
          ( model, Cmd.none )
        Just ( x, lines ) ->
          case
            notBeforeJust
              (Maybe.andThen identity << List.head << List.drop x)
              lines
          of
            Nothing ->
              ( model, Cmd.none )
            Just ( chord, _ ) ->
              ( model, focusById (toString chord.id) )

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

playStrum :
  Float -> Chord -> Int -> Float -> Float -> Schedule Int ->
    ( Float, Schedule Int, Cmd msg )
playStrum strumInterval chord id now start schedule =
  ( now
  , { stop = 12, segments = [ { x = id, start = 0 } ] }
  , AudioChange.playNotes
      3
      ( ( Maybe.map
            .x
            (Schedule.get (TickTime.toTick start now) schedule)
        ) /=
          Just id
      )
      now
      ( List.map
          ((+) now << (*) strumInterval << toFloat)
          (List.range 0 (List.length chord))
      )
      ( List.map
          (List.singleton << Chord.get chord)
          (List.range 0 (List.length chord))
      )
  )

playArpeggio :
  Chord -> Int -> Float -> Float -> Schedule Int ->
    ( Float, Schedule Int, Cmd msg )
playArpeggio chord id now start schedule =
  let
    wouldBeat = TickTime.nextBeat start now
  in let
    ( newStart, beat, truncatedSchedule, highStart, mute ) =
      case Schedule.get (wouldBeat - 1) schedule of
        Nothing ->
          ( now, 0, { stop = 0, segments = [] }, False, True )
        Just segment ->
          ( start
          , wouldBeat
          , Schedule.dropBefore (wouldBeat - 9) schedule
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
  in
    ( newStart
    , Schedule.add stop { x = id, start = beat } truncatedSchedule
    , AudioChange.playNotes
        1.5
        mute
        now
        ( List.map
            (TickTime.get newStart)
            (List.range beat (stop - 1))
        )
        (List.map (List.map (Chord.get chord)) arpeggio)
    )

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

notBeforeTrue : (a -> Bool) -> List a -> List a
notBeforeTrue pred xs =
  case xs of
    [] -> []
    x :: rest ->
      if pred x then xs
      else notBeforeTrue pred rest

notBeforeJust : (a -> Maybe b) -> List a -> Maybe ( b, List a )
notBeforeJust f xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      case f x of
        Just y -> Just ( y, rest )
        Nothing -> notBeforeJust f rest

findTrue : (a -> Bool) -> List a -> Maybe Int
findTrue = findTrueHelp 0

findTrueHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findTrueHelp i pred xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      if pred x then Just i
      else findTrueHelp (i + 1) pred rest

-- SUBSCRIPTIONS

port setChordBoxText : String -> Cmd msg

port focusById : String -> Cmd msg

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
    [ Html.Lazy.lazy2 viewPlayStyle model.strum model.strumInterval
    , Html.Lazy.lazy3 viewChordBox model.text model.parse model.chordBox
    , Html.Lazy.lazy viewSuggestionBar model.suggestionBar
    , Html.Lazy.lazy2 viewChordArea model.chordArea model.parse
    , Html.Lazy.lazy viewCircleOfFifths model.chordArea
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewPlayStyle : Bool -> Float -> Html Msg
viewPlayStyle strum strumInterval =
  div
    [ style
        [ ( "height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    ( List.concat
        [ [ span []
              [ Html.text "Play chords as " ]
          , button
              [ onClick (NeedsTime (SetStrum << (,) False))
              , classList
                  [ ( "pressMe", True )
                  , ( "chosen", not strum )
                  ]
              , style
                  [ ( "padding", "0px 3px" )
                  , ( "border-width", "1px" )
                  , ( "border-style", "solid" )
                  , ( "border-radius", "3px 0px 0px 3px" )
                  , ( "font", "inherit" )
                  , ( "height", "100%" )
                  ]
              ]
              [ Html.text "Arpeggio" ]
          , button
              [ onClick (NeedsTime (SetStrum << (,) True))
              , classList
                  [ ( "pressMe", True )
                  , ( "extension", True )
                  , ( "chosen", strum )
                  ]
              , style
                  [ ( "padding", "0px 3px" )
                  , ( "border-width", "1px" )
                  , ( "border-radius", "0px 3px 3px 0px" )
                  , ( "font", "inherit" )
                  , ( "height", "100%" )
                  , ( "margin-right", "5px" )
                  ]
              ]
              [ Html.text "Strum" ]
          ]
        , if strum then
            [ input
                [ type_ "range"
                , onInput SetStrumInterval
                , Html.Attributes.min "0"
                , Html.Attributes.max "0.1"
                , Html.Attributes.step "0.02"
                , Html.Attributes.value (toString strumInterval)
                , style
                    [ ( "height", "100%" )
                    , ( "box-sizing", "border-box" )
                    , ( "vertical-align", "top" )
                    , ( "margin", "0px" )
                    ]
                ]
                []
            , span
                [ style
                    [ ( "margin-left", "5px")
                    ]
                ]
                [ Html.text
                    (toString (strumInterval * 1000) ++ "ms between notes")
                ]
            ]
          else
            []
        ]
    )

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
        [ ( "font-size", "18pt" )
        , ( "margin-right", "5px" )
        , ( "margin-bottom", "5px" )
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
  in let
    play = NeedsTime (PlayChord << (,,) chord.cache.chord chord.id)
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
          [ onLeftDown play
          , onKeyDown
              [ ( 13, play )
              , ( 32, play )
              , ( 37, FocusHorizontal ( False, chord.id ) )
              , ( 38, FocusVertical ( False, chord.id ) )
              , ( 39, FocusHorizontal ( True, chord.id ) )
              , ( 40, FocusVertical ( True, chord.id ) )
              ]
          , id (toString chord.id)
          , style
              [ ( "background", CachedChord.bg chord.cache )
              , ( "color", CachedChord.fg chord.cache )
              , ( "font", "inherit" )
              , ( "width", "75px" )
              , ( "height", "75px" )
              , ( "padding", "0px 3px" )
              , ( "border"
                , "1px solid rgba(0, 0, 0, " ++
                    CachedChord.borderOpacity chord.cache ++ ")"
                )
              , ( "border-radius", "5px" )
              , ( "box-shadow"
                , "inset 18px 34px 20px -20px rgba(255, 255, 255, " ++
                    CachedChord.shineOpacity chord.cache ++ ")"
                )
              , ( "cursor", "pointer" )
              , ( "white-space", "nowrap" )
              ]
          ]
          (CachedChord.view chord.cache)
      ]

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

viewCircleOfFifths : ChordArea -> Html Msg
viewCircleOfFifths chordArea =
  Html.map
    msgFromCircleOfFifths
    (CircleOfFifths.view chordArea.activeChord chordArea.nextChord)

msgFromCircleOfFifths : CircleOfFifths.Msg -> Msg
msgFromCircleOfFifths msg =
  case msg of
    CircleOfFifths.PlayChord ( chord, id ) ->
      NeedsTime (PlayChord << (,,) chord id)
