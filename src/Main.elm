module Main exposing (..)

import AudioChange exposing (AudioChange(..), Note)
import AudioTime
import Buffet exposing (Buffet, LensChange)
import CachedChord
import ChordParser exposing (IdChord)
import CircleOfFifths
import CustomEvents exposing (onLeftDown, onKeyDown)
import Highlight exposing (Highlight)
import History exposing (History)
import MainParser
import Player exposing (Player, PlayStatus)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch
import UndoCatcher exposing (UndoCatcher)

import AnimationFrame
import Array exposing (Array)
import Dom
import Html exposing
  (Html, a, button, div, pre, span, text, textarea, input, select, option)
import Html.Attributes exposing
  (href, style, spellcheck, class, classList, id, type_, value, selected)
import Html.Events exposing (onClick, onInput)
import Html.Lazy
import Navigation exposing (Location)
import Task
import Url

main : Program Never Model Msg
main =
  Navigation.program
    UrlChanged
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
  , chordBox : ChordBox
  }

type PlayStyle
  = ArpeggioStyle
  | StrumStyle
  | PadStyle

type alias ChordLens =
  { lowestNote : Int
  , key : Int
  }

type alias ChordBox =
  { catcher : UndoCatcher
  , parse : MainParser.Model
  , buffet : Buffet
  }

init : Location -> ( Model, Cmd Msg )
init location =
  let
    text = textFromLocation location
  in let
    parse =
      MainParser.init CircleOfFifths.chordCount (Substring 0 text)
  in let
    key = parse.key
  in
    ( { player = { openings = [], schedule = [] }
      , history = { sequences = [], current = [] }
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , bpm = 85
      , chordLens =
          { lowestNote = 48
          , key = key
          }
      , home = True
      , chordBox =
          { catcher = UndoCatcher.fromString text
          , parse = parse
          , buffet =
              Buffet.fromSuggestions
                (MainParser.getSuggestions key parse)
          }
      }
    , Cmd.none
    )

textFromLocation : Location -> String
textFromLocation location =
  Maybe.withDefault defaultText (Url.hashParamValue "text" location)

defaultText : String
defaultText =
  "F   Csus4 C   G  G7\nDm7 FM7   _   E  E7\nDm  Asus4 Am  Em\nB0\n"

-- UPDATE

type Msg
  = NoOp
  | NeedsTime (Float -> Msg)
  | CurrentTime Float
  | PlayChord ( IdChord, Float )
  | StopChord Float
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | SetBpm String
  | SetLowestNote String
  | SetKey String
  | FocusHorizontal ( Bool, Int )
  | FocusVertical ( Bool, Int )
  | TextChanged String
  | UrlChanged Location
  | LensesChanged LensChange
  | Replace Suggestion
  | Undo
  | Redo

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

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
          Ok strumInterval ->
            { model | strumInterval = 0.001 * strumInterval }
          Err _ ->
            model
      , Cmd.none
      )

    SetBpm bpmString ->
      ( case String.toInt bpmString of
          Ok bpm -> { model | bpm = bpm }
          Err _ -> model
      , Cmd.none
      )

    SetLowestNote lowestNoteString ->
      ( case String.toInt lowestNoteString of
          Ok lowestNote ->
            let chordLens = model.chordLens in
              { model
              | chordLens = { chordLens | lowestNote = lowestNote + 48 }
              }
          Err _ ->
            model
      , Cmd.none
      )

    SetKey keyString ->
      ( case String.toInt keyString of
          Ok key ->
            let
              chordLens = model.chordLens
            in let
              chordBox = model.chordBox
            in
              { model
              | chordLens = { chordLens | key = key }
              , chordBox =
                  { chordBox
                  | buffet =
                      Buffet.changeSuggestions
                        (MainParser.getSuggestions key chordBox.parse)
                        chordBox.buffet
                  }
              }
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
          (MainParser.getChords model.chordBox.parse)
      of
        Just ( chord :: _, _ ) ->
          ( model
          , Task.attempt (always NoOp) (Dom.focus (toString chord.id))
          )
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
              ( model
              , Task.attempt (always NoOp) (Dom.focus (toString chord.id))
              )

    TextChanged newText ->
      ( { model
        | home = False
        , chordBox =
            mapCatcher
              model.chordLens.key
              (UndoCatcher.update newText)
              model.chordBox
        }
      , if model.home then
          Navigation.newUrl
            ("#text=" ++ Url.percentEncode newText)
        else
          Navigation.modifyUrl
            ("#text=" ++ Url.percentEncode newText)
      )

    UrlChanged location ->
      let newText = textFromLocation location in
        if newText /= model.chordBox.catcher.frame.text then
          ( { model
            | home = True
            , chordBox =
                mapCatcher
                  model.chordLens.key
                  ( UndoCatcher.replace
                      0
                      (String.length model.chordBox.catcher.frame.text)
                      newText
                  )
                  model.chordBox
            }
          , Task.attempt (always NoOp) (Dom.focus "catcher")
          )
        else
          ( model, Cmd.none )

    LensesChanged lensChange ->
      ( let chordBox = model.chordBox in
          { model
          | chordBox =
              { chordBox
              | buffet =
                  Buffet.changeLenses lensChange chordBox.buffet
              }
          }
      , Cmd.none
      )

    Replace suggestion ->
      ( case suggestion.ranges of
          [] ->
            model
          range :: _ ->
            { model
            | chordBox =
                mapCatcher
                  model.chordLens.key
                  ( UndoCatcher.replace
                      range.i
                      (Substring.stop range)
                      (Swatch.concat suggestion.swatches)
                  )
                  model.chordBox
            }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

    Undo ->
      ( { model
        | chordBox =
            mapCatcher model.chordLens.key UndoCatcher.undo model.chordBox
        }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

    Redo ->
      ( { model
        | chordBox =
            mapCatcher model.chordLens.key UndoCatcher.redo model.chordBox
        }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

mapCatcher : Int -> (UndoCatcher -> UndoCatcher) -> ChordBox -> ChordBox
mapCatcher key f chordBox =
  let
    catcher = f chordBox.catcher
  in let
    parse =
      MainParser.update (Substring 0 catcher.frame.text) chordBox.parse
  in
    { catcher = catcher
    , parse = parse
    , buffet =
        Buffet.changeSuggestions
          (MainParser.getSuggestions key parse)
          chordBox.buffet
    }

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

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    rest =
      [ UndoCatcher.undoPort (always Undo)
      , UndoCatcher.redoPort (always Redo)
      ]
  in
    if Player.willChange model.player then
      Sub.batch
        (AnimationFrame.times (always (NeedsTime CurrentTime)) :: rest)
    else
      Sub.batch rest


-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family", "Arial, Helvetica, sans-serif" )
        , ( "font-size", "85%" )
        ]
    ]
    [ span
        [ style
            [ ( "position", "relative" )
            , ( "display", "grid" )
            , ( "grid", """
"ps   ps   ps   ps   ps  "
"bpm  bpm  bpm  bpm  bpm "
"key  key  key  key  key "
"o1   o1   .    o2   o2  "
"ln1  ln2  ln2  ln2  ln3 "
"txt  txt  txt  txt  txt "
"buf  buf  buf  buf  buf "
/auto calc(6.5px + 0.5ch)
           auto calc(8.5px + 0.5ch)
                     1fr
"""
              )
            , ( "align-items", "center" )
            , ( "line-height", "2.2" )
            , ( "width", "37.5em" )
            , ( "white-space", "nowrap" )
            ]
        ]
        [ Html.Lazy.lazy2 viewPlayStyle model.playStyle model.strumInterval
        , Html.Lazy.lazy viewBpm model.bpm
        , Html.Lazy.lazy viewKey model.chordLens.key
        , span
            [ style
                [ ( "grid-area", "o1" )
                , ( "display", "flex" )
                , ( "justify-content", "space-between" )
                ]
            ]
            [ Html.text "Octave\xA0"
            , span [] [ Html.text "1" ]
            ]
        , input
            [ type_ "number"
            , value "2"
            , Html.Attributes.min "2"
            , Html.Attributes.max "2"
            , style
                [ ( "grid-area", "o2" )
                , ( "width", "3em" )
                ]
            ]
            []
        , span
            [ style
                [ ( "grid-area", "ln1" )
                ]
            ]
            [ Html.text "Lowest note\xA0"
            ]
        , Html.Lazy.lazy viewLowestNote model.chordLens.lowestNote
        , viewOctaveBrackets
        , Html.Lazy.lazy viewLowestNoteText model.chordLens.lowestNote
        , div
            [ style
                [ ( "grid-area", "txt" )
                , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
                , ( "font-size", "200%" )
                , ( "line-height", "initial" )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "bottom", "0" )
                ]
            ]
            [ Html.map
                TextChanged
                (Html.Lazy.lazy UndoCatcher.view model.chordBox.catcher)
            ]
        , Html.Lazy.lazy2 viewChordBox model.chordLens.key model.chordBox
        , Html.map
            interpretBuffetMessage
            (Html.Lazy.lazy Buffet.view model.chordBox.buffet)
        ]
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
  span
    [ style
        [ ( "grid-area", "ps" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        ]
    ]
    ( List.concat
        [ [ Html.text "Play chords as\xA0"
          , span
              [ class "radio"
              ]
              [ button
                  [ onClick (SetPlayStyle ArpeggioStyle)
                  , classList [ ( "chosen", playStyle == ArpeggioStyle ) ]
                  ]
                  [ Html.text "Arpeggio", span [] [], span [] [] ]
              , button
                  [ onClick (SetPlayStyle StrumStyle)
                  , classList [ ( "chosen", playStyle == StrumStyle ) ]
                  ]
                  [ Html.text "Strum", span [] [], span [] [] ]
              , button
                  [ onClick (SetPlayStyle PadStyle)
                  , classList [ ( "chosen", playStyle == PadStyle ) ]
                  ]
                  [ Html.text "Pad", span [] [], span [] [] ]
              ]
          ]
        , if playStyle == StrumStyle then
            [ Html.text "\xA0"
            , input
                [ type_ "range"
                , onInput SetStrumInterval
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , Html.Attributes.step "20"
                , value (toString (1000 * strumInterval))
                , style
                    [ ( "width", "auto" )
                    , ( "min-width", "7em" )
                    ]
                ]
                []
            , Html.text "\xA0"
            , span
                [ style
                    [ ( "line-height", "1.25" )
                    , ( "white-space", "normal" )
                    ]
                ]
                [ Html.text
                    (toString (1000 * strumInterval) ++ "ms between notes")
                ]
            ]
          else
            []
        ]
    )

viewBpm : Int -> Html Msg
viewBpm bpm =
  span
    [ style
        [ ( "grid-area", "bpm" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        ]
    ]
    [ span []
        [ Html.text "Tempo\xA0" ]
    , input
        [ type_ "range"
        , onInput SetBpm
        , value (toString bpm)
        , Html.Attributes.size 3
        , Html.Attributes.min "60"
        , Html.Attributes.max "140"
        , Html.Attributes.step "5"
        , style
            [ ( "width", "auto" )
            , ( "min-width", "7em" )
            ]
        ]
        []
    , span []
        [ Html.text ("\xA0" ++ toString bpm ++ " BPM") ]
    ]

viewKey : Int -> Html Msg
viewKey key =
  span
    [ style
        [ ( "grid-area", "key" )
        ]
    ]
    [ Html.text "Key signature "
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

viewOctaveBrackets : Html msg
viewOctaveBrackets =
  span
    [ style
        [ ( "grid-area", "ln2" )
        , ( "display", "flex" )
        , ( "position", "absolute" )
        , ( "top", "-0.6em" )
        , ( "bottom", "calc(50% + 11px)" )
        , ( "left", "calc(6px - 1ch)" )
        , ( "right", "calc(6px - 1ch)" )
        , ( "pointer-events", "none" )
        ]
    ]
    [ span
        [ style
            [ ( "flex", "5 0 1ch" )
            , ( "border-top", "1px solid" )
            , ( "border-right", "1px solid" )
            ]
        ]
        []
    , span
        [ style
            [ ( "flex", "1" )
            ]
        ]
        []
    , span
        [ style
            [ ( "flex", "6 0 1ch" )
            , ( "border-top", "1px solid" )
            , ( "border-left", "1px solid" )
            ]
        ]
        []
    ]

viewLowestNote : Int -> Html Msg
viewLowestNote lowestNote =
  input
    [ type_ "range"
    , onInput SetLowestNote
    , Html.Attributes.min "-6"
    , Html.Attributes.max "6"
    , value (toString (lowestNote - 48))
    , style
        [ ( "grid-area", "ln2" )
        , ( "width", "auto" )
        , ( "min-width", "7em" )
        ]
    ]
    []


viewLowestNoteText : Int -> Html Msg
viewLowestNoteText lowestNote =
  let
    octaveOffset = lowestNote % 12
  in let
    octave = (lowestNote - octaveOffset) // 12 - 2
  in
    span
      [ style [ ( "grid-area", "ln3" ) ]
      ]
      [ Html.text
          ( case Array.get octaveOffset flatNames of
              Nothing -> "error"
              Just flatName -> "\xA0" ++ flatName ++ toString octave
          )
      ]

flatNames : Array String
flatNames =
  Array.fromList
    [ "C", "D♭", "D", "E♭", "E", "F", "G♭", "G", "A♭", "A", "B♭", "B" ]

viewChordBox : Int -> ChordBox -> Html Msg
viewChordBox key chordBox =
  pre
    [ style
        [ ( "grid-area", "txt" )
        , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
        , ( "font-size", "200%" )
        , ( "line-height", "initial" )
        , ( "padding", "10px" )
        , ( "border", "2px solid")
        , ( "margin", "0" )
        , ( "white-space", "pre-wrap" )
        , ( "word-wrap", "break-word" )
        , ( "color", "transparent" )
        ]
    ]
    ( List.map
        Swatch.view
        ( Highlight.mergeLayers
            [ Buffet.highlights chordBox.buffet
            , MainParser.view key chordBox.parse
            , [ Highlight
                  "#000000"
                  "#ffffff"
                  (Substring 0 (chordBox.catcher.frame.text ++ "\n"))
              ]
            ]
        )
    )

interpretBuffetMessage : Buffet.Msg -> Msg
interpretBuffetMessage buffetMessage =
  case buffetMessage of
    Buffet.LensesChanged lensChange ->
      LensesChanged lensChange
    Buffet.Replace suggestion ->
      Replace suggestion

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
            CachedChord.transposeRootOctave chordLens.lowestNote chord.cache
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
        chordLens.lowestNote
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
