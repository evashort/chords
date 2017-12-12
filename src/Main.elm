port module Main exposing (..)

import AudioChange exposing (AudioChange(..), Note)
import AudioTime
import CachedChord
import ChordParser exposing (IdChord)
import CircleOfFifths
import BubbleSwatch
import CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)
import Highlight exposing (Highlight)
import History exposing (History)
import MainParser
import Player exposing (Player, PlayStatus)
import Selection
import Substring exposing (Substring)
import SuggestionBar

import AnimationFrame
import Array
import Html exposing
  (Html, a, button, div, pre, span, text, textarea, input, select, option)
import Html.Attributes exposing
  (href, style, spellcheck, id, classList, type_, value, selected)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Html.Lazy
import Navigation exposing (Location)
import Task exposing (Task)
import Time
import Url

main : Program Bool Model Msg
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
  { player : Player
  , history : History
  , playStyle : PlayStyle
  , strumInterval : Float
  , bpm : Int
  , chordLens : ChordLens
  , home : Bool
  , subscribeToSelection : Bool
  , chordBoxFocused : Bool
  , bubble : Maybe Highlight
  , chordBox : ChordBox
  , suggestionBar : SuggestionBar.Model
  }

type PlayStyle
  = ArpeggioStyle
  | StrumStyle
  | PadStyle

type alias ChordLens =
  { octaveBase : Int
  , key : Int
  }

type alias ChordBox =
  { text : String
  , parse : MainParser.Model
  , highlightRanges : List Substring
  , bubble : Maybe Highlight
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
    ( { player = { openings = [], schedule = [] }
      , history = { sequences = [], current = [] }
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , bpm = 85
      , chordLens =
          { octaveBase = 48
          , key = 0
          }
      , home = True
      , subscribeToSelection = True
      , chordBoxFocused = True
      , bubble = Nothing
      , chordBox =
          { text = text
          , parse = parse
          , highlightRanges =
              SuggestionBar.highlightRanges suggestionBar
          , bubble = Nothing
          }
      , suggestionBar = suggestionBar
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
  | PlayChord ( IdChord, Float )
  | StopChord Float
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | SetBpm String
  | SetOctaveBase String
  | SetKey String
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
      ( case Player.setTime now model.player of
          Nothing -> model
          Just ( player, sequenceFinished ) ->
            { model
            | player = player
            , history =
                if sequenceFinished then
                  History.finishSequence model.history
                else
                  model.history
            }
      , Cmd.none
      )

    PlayChord ( chord, now ) ->
      let
        x = chord.cache.chord
      in let
        ( player, sequenceFinished ) =
          Maybe.withDefault
            ( model.player, False )
            (Player.setTime now model.player)
      in let
        newHistory =
          History.add
            chord.cache
            ( if sequenceFinished then
                History.finishSequence model.history
              else
                model.history
            )
      in let
        ( newPlayer, changes ) =
          case model.playStyle of
            ArpeggioStyle ->
              Player.playArpeggio
                (60 / toFloat model.bpm) x chord.id now player
            StrumStyle ->
              Player.playStrum model.strumInterval x chord.id now player
            PadStyle ->
              Player.playPad x chord.id now player
      in
        ( { model | player = newPlayer, history = newHistory }
        , AudioChange.perform changes
        )

    StopChord now ->
      let
        ( player, changes ) =
          Player.stopPlaying now model.player
      in
        ( { model
          | player = player
          , history = History.finishSequence model.history
          }
        , AudioChange.perform changes
        )

    SetPlayStyle playStyle ->
      ( if playStyle == model.playStyle then model
        else { model | playStyle = playStyle }
      , Cmd.none
      )

    SetStrumInterval strumIntervalString ->
      ( case String.toFloat strumIntervalString of
          Ok strumInterval -> { model | strumInterval = strumInterval }
          Err _ -> model
      , Cmd.none
      )

    SetBpm bpmString ->
      ( case String.toInt bpmString of
          Ok bpm -> { model | bpm = bpm }
          Err _ -> model
      , Cmd.none
      )

    SetOctaveBase octaveBaseString ->
      ( case String.toInt octaveBaseString of
          Ok octaveBase ->
            let chordLens = model.chordLens in
              { model
              | chordLens = { chordLens | octaveBase = octaveBase }
              }
          Err _ ->
            model
      , Cmd.none
      )

    SetKey keyString ->
      ( case String.toInt keyString of
          Ok key ->
            let chordLens = model.chordLens in
              { model
              | chordLens = { chordLens | key = key }
              }
          Err _ -> model
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
          (MainParser.getChords model.chordBox.parse)
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
              (MainParser.getChords model.chordBox.parse)
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
        chordBox = model.chordBox
      in let
        parse = MainParser.update (Substring 0 newText) chordBox.parse
      in let
        suggestions = MainParser.getSuggestions parse
      in
        updateSuggestionBar
          (SuggestionBar.SuggestionsChanged suggestions)
          { model
          | chordBox =
              { chordBox | text = newText, parse = parse }
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
        if newText /= model.chordBox.text then
          let
            chordBox = model.chordBox
          in let
            parse = MainParser.update (Substring 0 newText) chordBox.parse
          in let
            suggestions = MainParser.getSuggestions parse
          in
            updateSuggestionBar
              (SuggestionBar.SuggestionsChanged suggestions)
              { model
              | chordBox =
                  { chordBox | text = newText, parse = parse }
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
        , if Player.willChange model.player then
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
    [ Html.Lazy.lazy2 viewPlayStyle model.playStyle model.strumInterval
    , Html.Lazy.lazy viewBpm model.bpm
    , Html.Lazy.lazy viewOctaveBase model.chordLens.octaveBase
    , Html.Lazy.lazy viewKey model.chordLens.key
    , Html.Lazy.lazy2 viewChordBox model.chordLens.key model.chordBox
    , Html.Lazy.lazy2
        viewSuggestionBar model.chordLens.key model.suggestionBar
    , Html.Lazy.lazy3
        viewChordArea model.chordLens model.player model.chordBox.parse
    , Html.Lazy.lazy2 viewCircleOfFifths model.chordLens model.player
    , Html.Lazy.lazy2
        History.view model.chordLens.key model.history.sequences
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewPlayStyle : PlayStyle -> Float -> Html Msg
viewPlayStyle playStyle strumInterval =
  div
    [ style
        [ ( "line-height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    ( List.concat
        [ [ span []
              [ Html.text "Play chords as " ]
          , button
              [ onClick (SetPlayStyle ArpeggioStyle)
              , classList
                  [ ( "pressMe", True )
                  , ( "extended", True )
                  , ( "chosen", playStyle == ArpeggioStyle )
                  ]
              , style
                  [ ( "padding", "0px 3px" )
                  , ( "border-width", "1px" )
                  , ( "border-style", "solid" )
                  , ( "border-radius", "3px 0px 0px 3px" )
                  , ( "font", "inherit" )
                  , ( "line-height", "24px" )
                  ]
              ]
              [ Html.text "Arpeggio" ]
          , button
              [ onClick (SetPlayStyle StrumStyle)
              , classList
                  [ ( "pressMe", True )
                  , ( "extension", True )
                  , ( "middle", True )
                  , ( "chosen", playStyle == StrumStyle )
                  ]
              , style
                  [ ( "padding", "0px 3px" )
                  , ( "border-width", "1px" )
                  , ( "font", "inherit" )
                  , ( "line-height", "24px" )
                  ]
              ]
              [ Html.text "Strum" ]
          , button
              [ onClick (SetPlayStyle PadStyle)
              , classList
                  [ ( "pressMe", True )
                  , ( "extension", True )
                  , ( "chosen", playStyle == PadStyle )
                  ]
              , style
                  [ ( "padding", "0px 3px" )
                  , ( "border-width", "1px" )
                  , ( "border-radius", "0px 3px 3px 0px" )
                  , ( "font", "inherit" )
                  , ( "line-height", "24px" )
                  ]
              ]
              [ Html.text "Pad" ]
          ]
        , if playStyle == StrumStyle then
            [ input
                [ type_ "range"
                , onInput SetStrumInterval
                , Html.Attributes.min "0"
                , Html.Attributes.max "0.1"
                , Html.Attributes.step "0.02"
                , value (toString strumInterval)
                , style
                    [ ( "margin", "0px 5px" )
                    , ( "height", "26px" )
                    , ( "vertical-align", "top" )
                    , ( "box-sizing", "border-box" )
                    ]
                ]
                []
            , span []
                [ Html.text
                    (toString (strumInterval * 1000) ++ "ms between notes")
                ]
            ]
          else
            []
        ]
    )

viewBpm : Int -> Html Msg
viewBpm bpm =
  div
    [ style
        [ ( "line-height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    [ span []
        [ Html.text "Tempo " ]
    , input
        [ type_ "number"
        , onInput SetBpm
        , value (toString bpm)
        , Html.Attributes.size 3
        , Html.Attributes.min "60"
        , Html.Attributes.max "120"
        , style
            [ ( "width", "4em" )
            ]
        ]
        []
    , span []
        [ Html.text " BPM" ]
    ]

viewOctaveBase : Int -> Html Msg
viewOctaveBase octaveBase =
  div
    [ style
        [ ( "line-height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    [ span []
        [ Html.text "Root octave " ]
    , input
        [ type_ "range"
        , onInput SetOctaveBase
        , Html.Attributes.min "40"
        , Html.Attributes.max "53"
        , value (toString octaveBase)
        , style
            [ ( "margin", "0px 5px" )
            , ( "height", "26px" )
            , ( "vertical-align", "top" )
            , ( "box-sizing", "border-box" )
            ]
        ]
        []
    , span []
        [ Html.text
            ( let
                octaveOffset = octaveBase % 12
              in let
                octave = (octaveBase - octaveOffset) // 12 - 2
              in
                String.concat
                  [ getFlatName octaveOffset
                  , toString octave
                  , " through "
                  , getSharpName ((octaveOffset + 11) % 12)
                  , toString (octave + min octaveOffset 1)
                  ]
            )
        ]
    ]

getSharpName : Int -> String
getSharpName note =
  Maybe.withDefault
    ""
    ( Array.get
        note
        ( Array.fromList
            [ "C", "C♯", "D", "D♯", "E"
            , "F", "F♯", "G", "G♯", "A", "A♯", "B"
            ]
        )
    )

getFlatName : Int -> String
getFlatName note =
  Maybe.withDefault
    ""
    ( Array.get
        note
        ( Array.fromList
            [ "C", "D♭", "D", "E♭", "E"
            , "F", "G♭", "G", "A♭", "A", "B♭", "B"
            ]
        )
    )

viewKey : Int -> Html Msg
viewKey key =
  div
    [ style
        [ ( "line-height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    [ span []
        [ Html.text "Key signature " ]
    , select
        [ onInput SetKey
        ]
        [ option [ value "0", selected (key == 0) ] [ Html.text "C / Am" ]
        , option [ value "7", selected (key == 7) ] [ Html.text "G / Em" ]
        , option [ value "2", selected (key == 2) ] [ Html.text "D / Bm" ]
        , option [ value "9", selected (key == 9) ] [ Html.text "A / F♯m" ]
        , option [ value "4", selected (key == 4) ] [ Html.text "E / C♯m" ]
        , option [ value "11", selected (key == 11) ] [ Html.text "B / G♯m" ]
        , option [ value "6", selected (key == 6) ] [ Html.text "G♭ / E♭m" ]
        , option [ value "1", selected (key == 1) ] [ Html.text "D♭ / B♭m" ]
        , option [ value "8", selected (key == 8) ] [ Html.text "A♭ / Fm" ]
        , option [ value "3", selected (key == 3) ] [ Html.text "E♭ / Cm" ]
        , option [ value "10", selected (key == 10) ] [ Html.text "B♭ / Gm" ]
        , option [ value "5", selected (key == 5) ] [ Html.text "F / Dm" ]
        ]
    ]

viewChordBox : Int -> ChordBox -> Html Msg
viewChordBox key chordBox =
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
        [ text chordBox.text ]
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
            BubbleSwatch.view
            (Highlight.mergeLayers (getLayers key chordBox))
        )
    ]

getLayers : Int -> ChordBox -> List (List Highlight)
getLayers key chordBox =
  [ let
      grays =
        List.map
          (Highlight "" "#ffffff" "#aaaaaa")
          chordBox.highlightRanges
    in
      case chordBox.bubble of
        Just bubble -> bubble :: grays
        Nothing -> grays
  , MainParser.view key chordBox.parse
  , [ Highlight
        ""
        "#000000"
        "#ffffff"
        (Substring 0 (chordBox.text ++ "\n"))
    ]
  ]

viewSuggestionBar : Int -> SuggestionBar.Model -> Html Msg
viewSuggestionBar key suggestionBar =
  Html.map SuggestionBarMsg (SuggestionBar.view key suggestionBar)

viewChordArea : ChordLens -> Player -> MainParser.Model -> Html Msg
viewChordArea chordLens player parse =
  div
    [ style
        [ ( "font-size", "18pt" )
        , ( "margin-right", "5px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    ( List.map
        (viewLine chordLens (Player.playStatus player))
        (MainParser.getChords parse)
    )

viewLine : ChordLens -> PlayStatus -> List (Maybe IdChord) -> Html Msg
viewLine chordLens playStatus line =
  div
    [ style
        [ ( "display", "flex" ) ]
    ]
    (List.map (viewMaybeChord chordLens playStatus) line)

viewMaybeChord : ChordLens -> PlayStatus -> Maybe IdChord -> Html Msg
viewMaybeChord chordLens playStatus maybeChord =
  case maybeChord of
    Just chord ->
      viewChord
        chordLens.key
        playStatus
        { chord
        | cache =
            CachedChord.transposeRootOctave chordLens.octaveBase chord.cache
        }
    Nothing ->
      viewSpace

viewChord : Int -> PlayStatus -> IdChord -> Html Msg
viewChord key playStatus chord =
  let
    selected =
      playStatus.active == chord.id || playStatus.next == chord.id
  in let
    stopButton = playStatus.active == chord.id && playStatus.stoppable
  in let
    play =
      if stopButton then NeedsTime StopChord
      else NeedsTime (PlayChord << (,) chord)
  in
    span
      [ style
          [ ( "border-style"
            , if playStatus.next == chord.id then
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
              [ ( "background", CachedChord.bg key chord.cache )
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
          ( if stopButton then
              [ Html.span
                  [ style
                     [ ( "background", CachedChord.fg chord.cache )
                     , ( "width", "20px" )
                     , ( "height", "20px" )
                     , ( "display", "inline-block" )
                     , ( "vertical-align", "middle" )
                     ]
                  ]
                  []
              ]
            else
              CachedChord.view chord.cache
          )
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

viewCircleOfFifths : ChordLens -> Player -> Html Msg
viewCircleOfFifths chordLens player =
  Html.map
    msgFromCircleOfFifths
    ( CircleOfFifths.view
        chordLens.octaveBase
        chordLens.key
        (Player.playStatus player)
    )

msgFromCircleOfFifths : CircleOfFifths.Msg -> Msg
msgFromCircleOfFifths msg =
  case msg of
    CircleOfFifths.PlayChord chord ->
      NeedsTime (PlayChord << (,) chord)
    CircleOfFifths.StopChord ->
      NeedsTime StopChord
