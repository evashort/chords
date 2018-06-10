module Main exposing (..)

import AudioChange
import AudioTime
import Buffet exposing (Buffet, LensChange)
import CircleOfFifths
import CustomEvents exposing (onChange)
import Highlight exposing (Highlight)
import History exposing (History)
import LowestNote
import Parse exposing (Parse)
import Player exposing (Player)
import PlayStatus exposing (PlayStatus, IdChord)
import Ports
import Radio
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Song
import Substring exposing (Substring)
import Swatch
import Theater

import AnimationFrame
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, textarea, input
  , select, option
  )
import Html.Attributes as Attributes exposing
  (href, style, id, type_, value, selected, disabled)
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
  { bpm : Maybe Float
  , lowestNote : Maybe Int
  , parse : Parse
  , memory : Maybe Backup
  , saved : Bool
  , buffet : Buffet
  , playStyle : PlayStyle
  , strumInterval : Float
  , player : Player
  , pane : Pane
  , history : History
  }

type PlayStyle
  = ArpeggioStyle
  | StrumStyle
  | PadStyle

type Pane
  = FifthsPane
  | HistoryPane

type alias Backup =
  { code : String
  , action : String
  }

init : Location -> ( Model, Cmd Msg )
init location =
  let
    maybeText = Url.hashParamValue "text" location
  in let
    text = Maybe.withDefault defaultText maybeText
  in let
    parse = Parse.init CircleOfFifths.chordCount text
  in
    ( { bpm = Nothing
      , lowestNote = Nothing
      , parse = parse
      , memory = Nothing
      , saved = maybeText /= Nothing
      , buffet = Buffet.init parse.suggestions
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , player = { openings = [], schedule = [] }
      , pane = FifthsPane
      , history = { sequences = [], current = [] }
      }
    , Cmd.batch
        [ Theater.init
            { text = text
            , selectionStart = String.length text
            , selectionEnd = String.length text
            }
        , Theater.focus
        ]
    )

defaultText : String
defaultText =
  "F   Csus4 C   G  G7\nDm7 FM7   _   E  E7\nDm  Asus4 Am  Em\nB0\n"

-- UPDATE

type Msg
  = PreviewBpm String
  | SetBpm String
  | PreviewLowestNote String
  | SetLowestNote String
  | SetKey String
  | TextChanged String
  | UrlChanged Location
  | Save
  | BuffetMsg Buffet.Msg
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | RequestTime
  | CurrentTime Float
  | PlayStatusMsg PlayStatus.Msg
  | Play (IdChord, Float)
  | Stop Float
  | SetPane Pane

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PreviewBpm bpmString ->
      ( case String.toFloat bpmString of
          Ok bpm ->
            { model | bpm = Just bpm }
          Err _ ->
            model
      , Cmd.none
      )

    SetBpm bpmString ->
      case String.toFloat bpmString of
        Ok bpm ->
          doAction
            "bpm"
            (Parse.setBpm bpm)
            { model | bpm = Nothing }
        Err _ ->
          ( model, Cmd.none )

    PreviewLowestNote lowestNoteString ->
      ( case String.toInt lowestNoteString of
          Ok lowestNote ->
            { model | lowestNote = Just lowestNote }
          Err _ ->
            model
      , Cmd.none
      )

    SetLowestNote lowestNoteString ->
      case String.toInt lowestNoteString of
        Ok lowestNote ->
          doAction
            "lowestNote"
            (Parse.setLowestNote lowestNote)
            { model | lowestNote = Nothing }
        Err _ ->
          ( model, Cmd.none )

    SetKey keyString ->
      case String.toInt keyString of
        Ok key ->
          let
            scale =
              if model.parse.scale.minor then
                { minor = True, root = (key - 3) % 12 }
              else
                { minor = False, root = key }
          in
            doAction "key" (Parse.setScale scale) model
        Err _ ->
          ( model, Cmd.none )

    TextChanged code ->
      ( let parse = Parse.update code model.parse in
          { model
          | parse = parse
          , memory = Nothing
          , saved = False
          , buffet =
              Buffet.update parse.suggestions model.buffet
          }
      , clearUrl model.saved
      )

    UrlChanged location ->
      case Url.hashParamValue "text" location of
        Nothing ->
          ( model, Cmd.none )
        Just code ->
          if code == model.parse.code then
            ( { model | saved = True }, Cmd.none )
          else
            ( let parse = Parse.update code model.parse in
                { model
                | parse = parse
                , memory = Nothing
                , saved = True
                , buffet =
                    Buffet.update parse.suggestions model.buffet
                }
            , Cmd.batch
                [ Theater.replace
                    { old = Substring 0 model.parse.code
                    , new = code
                    }
                , Theater.focus
                ]
            )

    Save ->
      ( model
      , Navigation.modifyUrl
          ("#text=" ++ Url.percentEncode model.parse.code)
      )

    BuffetMsg (Buffet.LensesChanged lensChange) ->
      ( { model
        | buffet = Buffet.changeLenses lensChange model.buffet
        }
      , Cmd.none
      )

    BuffetMsg (Buffet.Replace suggestion) ->
      case suggestion.ranges of
        [] ->
          ( model, Cmd.none )
        range :: _ ->
          let
            replacement =
              { old = range
              , new = Swatch.concat suggestion.swatches
              }
          in let
            code = Replacement.apply replacement model.parse.code
          in
            ( let parse = Parse.update code model.parse in
                { model
                | parse = parse
                , memory = Nothing
                , saved = False
                , buffet =
                    Buffet.update parse.suggestions model.buffet
                }
            , Cmd.batch
                [ Theater.replace replacement
                , Theater.focus
                , clearUrl model.saved
                ]
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

    RequestTime ->
      ( model, Task.perform CurrentTime AudioTime.now )

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

    PlayStatusMsg (PlayStatus.Play idChord) ->
      ( model, Task.perform (Play << (,) idChord) AudioTime.now )

    PlayStatusMsg PlayStatus.Stop ->
      ( model, Task.perform Stop AudioTime.now )

    Play ( idChord, now ) ->
      let
        ( player, sequenceFinished ) =
          Maybe.withDefault
            ( model.player, False )
            (Player.setTime now model.player)
      in let
        newHistory =
          History.add
            idChord.chord
            ( if sequenceFinished then
                History.finishSequence model.history
              else
                model.history
            )
      in let
        lowestNote = model.parse.lowestNote
      in let
        ( newPlayer, changes ) =
          case model.playStyle of
            ArpeggioStyle ->
              Player.arp
                (60 / model.parse.bpm) lowestNote idChord now player
            StrumStyle ->
              Player.strum
                model.strumInterval lowestNote idChord now player
            PadStyle ->
              Player.pad lowestNote idChord now player
      in
        ( { model | player = newPlayer, history = newHistory }
        , AudioChange.perform changes
        )

    Stop now ->
      let
        ( player, changes ) =
          Player.stop now model.player
      in
        ( { model
          | player = player
          , history = History.finishSequence model.history
          }
        , AudioChange.perform changes
        )

    SetPane pane ->
      ( { model | pane = pane }
      , Cmd.none
      )

doAction :
  String -> (String -> Maybe Replacement) -> Model -> ( Model, Cmd msg )
doAction action f model =
  let
    oldCode =
      case model.memory of
        Nothing ->
          model.parse.code
        Just backup ->
          if backup.action == action then
            backup.code
          else
            model.parse.code
  in
    case f oldCode of
      Nothing ->
        case model.memory of
          Nothing ->
            ( model, Cmd.none )
          Just backup ->
            if backup.action == action then
              ( let parse = Parse.update oldCode model.parse in
                  { model
                  | parse = parse
                  , memory = Nothing
                  , saved = False
                  , buffet =
                      Buffet.update parse.suggestions model.buffet
                  }
              , Cmd.batch
                  [ Theater.hardUndo, clearUrl model.saved ]
              )
            else
              ( { model | memory = Nothing }, Cmd.none )
      Just replacement ->
        let code = Replacement.apply replacement oldCode in
          ( let parse = Parse.update code model.parse in
              { model
              | parse = parse
              , memory = Just { action = action, code = oldCode }
              , saved = False
              , buffet =
                  Buffet.update parse.suggestions model.buffet
              }
          , Cmd.batch
              [ case model.memory of
                  Nothing ->
                    Theater.replace replacement
                  Just backup ->
                    if backup.action == action then
                      Theater.undoAndReplace replacement
                    else
                      Theater.replace replacement
              , clearUrl model.saved
              ]
          )

clearUrl : Bool -> Cmd msg
clearUrl saved =
  if saved then
    Navigation.newUrl "#"
  else
    Cmd.none

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if Player.willChange model.player then
    Sub.batch
      [ Ports.text TextChanged
      , AnimationFrame.times (always RequestTime)
      ]
  else
    Ports.text TextChanged

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family"
          , "Arial, \"Helvetica Neue\", Helvetica, sans-serif"
          )
        , ( "line-height", "1.9" )
        , ( "white-space", "nowrap" )
        , ( "position", "relative" )
        , ( "display", "grid" )
        , ( "grid", """
"title ."
"bpm ."
"lowestNote ."
"key ."
"theater save"
"buffet buffet"
"playStyle playStyle"
"song song"
"paneSelector paneSelector"
"pane pane"
/ minmax(auto, 37.5em) 1fr
"""
          )
        , ( "align-items", "center" )
        ]
    ]
    [ viewTitle
    , Html.Lazy.lazy2
        viewBpm
        (hasBackup "bpm" model)
        (Maybe.withDefault model.parse.bpm model.bpm)
    , Html.Lazy.lazy2
        viewLowestNote
        (hasBackup "lowestNote" model)
        (Maybe.withDefault model.parse.lowestNote model.lowestNote)
    , Html.Lazy.lazy2
        viewKey
        (hasBackup "key" model)
        model.parse.scale
    , div
        [ id "theater"
        , style
            [ ( "grid-area", "theater" )
            , ( "font-family"
              , "\"Lucida Console\", Monaco, monospace"
              )
            , ( "font-size", "160%" )
            , ( "line-height", "initial" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "bottom", "0" )
            ]
        ]
        []
    , Html.Lazy.lazy2 viewHighlights model.parse model.buffet
    , Html.Lazy.lazy viewBuffet model.buffet
    , button
        [ onClick Save
        , disabled model.saved
        , style
            [ ( "grid-area", "save" )
            , ( "justify-self", "start")
            , ( "align-self", "end" )
            , ( "margin-left", "8px" )
            ]
        ]
        [ Html.text "Save in URL"
        ]
    , Html.Lazy.lazy2
        viewPlayStyle
        model.playStyle
        model.strumInterval
    , Html.Lazy.lazy2 viewSong model.player model.parse
    , Html.Lazy.lazy
        viewPaneSelector
        model.pane
    , case model.pane of
        FifthsPane ->
          Html.Lazy.lazy2
            viewCircleOfFifths
            model.parse.scale
            model.player
        HistoryPane ->
          Html.Lazy.lazy2
            viewHistory
              model.parse.scale
              model.history
    ]

hasBackup : String -> Model -> Bool
hasBackup action model =
  case model.memory of
    Nothing ->
      False
    Just backup ->
      backup.action == action

viewTitle : Html msg
viewTitle =
  span
    [ style
        [ ( "grid-area", "title" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "space-between" )
        ]
    ]
    [ span
        [ style
            [ ( "font-size", "150%" )
            , ( "line-height", "initial" )
            ]
        ]
        [ Html.text "Chord progression editor"
        ]
    , a
        [ href "https://github.com/evanshort73/chords"
        ]
        [ Html.text "View on GitHub"
        ]
    ]


viewBpm : Bool -> Float -> Html Msg
viewBpm hasBackup bpm =
  span
    [ style
        [ ( "grid-area", "bpm" )
        , yellowIf hasBackup
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "margin-top", "8px" )
        ]
    ]
    [ span []
        [ Html.text "Tempo\xA0" ]
    , input
        [ type_ "range"
        , onInput PreviewBpm
        , onChange SetBpm
        , value (toString bpm)
        , Attributes.size 3
        , Attributes.min "60"
        , Attributes.max "140"
        , Attributes.step "5"
        , style
            [ ( "width", "9.5em" )
            ]
        ]
        []
    , span []
        [ Html.text ("\xA0" ++ toString bpm ++ " BPM") ]
    ]

viewKey : Bool -> Scale -> Html Msg
viewKey hasBackup scale =
  let
    key =
      if scale.minor then
        (scale.root + 3) % 12
      else
        scale.root
  in
    span
      [ style
          [ ( "grid-area", "key" )
          , yellowIf hasBackup
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

viewLowestNote : Bool -> Int -> Html Msg
viewLowestNote hasBackup lowestNote =
  span
    [ style
        [ ( "grid-area", "lowestNote" )
        , yellowIf hasBackup
        , ( "display", "flex" )
        , ( "align-items", "center" )
        ]
    ]
    [ span []
        [ Html.text "Lowest note\xA0" ]
    , input
        [ type_ "range"
        , onInput PreviewLowestNote
        , onChange SetLowestNote
        , value (toString lowestNote)
        , Attributes.min "35"
        , Attributes.max "53"
        , style
            [ ( "width", "10em" )
            ]
        ]
        []
    , span
        [ style
            [ ( "min-width", "4ch" )
            ]
        ]
        [ Html.text ("\xA0" ++ LowestNote.view lowestNote) ]
    , span []
        [ Html.text ("\xA0(MIDI note " ++ toString lowestNote ++ ")") ]
    ]

yellowIf : Bool -> (String, String)
yellowIf condition =
  ( "background", if condition then "#fafac0" else "inherit" )

viewHighlights : Parse -> Buffet -> Html Msg
viewHighlights parse buffet =
  pre
    [ style
        [ ( "grid-area", "theater" )
        , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
        , ( "font-size", "160%" )
        , ( "line-height", "initial" )
        , ( "padding", "8px" )
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
            [ Buffet.highlights buffet
            , parse.highlights
            , [ Highlight
                  "#000000"
                  "#ffffff"
                  (Substring 0 (parse.code ++ "\n"))
              ]
            ]
        )
    )

viewBuffet : Buffet -> Html Msg
viewBuffet buffet =
  Html.map BuffetMsg (Buffet.view buffet)

viewPlayStyle : PlayStyle -> Float -> Html Msg
viewPlayStyle playStyle strumInterval =
  span
    [ style
        [ ( "grid-area", "playStyle" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "margin-top", "8px" )
        ]
    ]
    ( List.concat
        [ [ Html.text "Play chords as\xA0"
          , Html.map
              SetPlayStyle
              ( Radio.view
                  playStyle
                  [ ( "Arpeggio", ArpeggioStyle )
                  , ( "Strum", StrumStyle )
                  , ( "Pad", PadStyle )
                  ]
              )
          ]
        , if playStyle == StrumStyle then
            [ Html.text "\xA0"
            , input
                [ type_ "range"
                , onInput SetStrumInterval
                , Attributes.min "0"
                , Attributes.max "100"
                , Attributes.step "20"
                , value (toString (1000 * strumInterval))
                , style
                    [ ( "width", "5em" )
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

viewSong : Player -> Parse -> Html Msg
viewSong player parse =
  Html.map
    PlayStatusMsg
    ( Song.view
        "song"
        parse.scale.root
        (Player.status player)
        (Parse.song parse)
    )

viewPaneSelector : Pane -> Html Msg
viewPaneSelector pane =
  span
    [ style
        [ ( "grid-area", "paneSelector" )
        , ( "margin-top", "8px" )
        ]
    ]
    [ Html.map
        SetPane
        ( Radio.view
            pane
            [ ( "Circle of fifths", FifthsPane )
            , ( "Recently played", HistoryPane )
            ]
        )
    ]

viewCircleOfFifths : Scale -> Player -> Html Msg
viewCircleOfFifths scale player =
  let
    key =
      if scale.minor then
        (scale.root + 3) % 12
      else
        scale.root
  in
    Html.map
      PlayStatusMsg
      (CircleOfFifths.view "pane" key (Player.status player))

viewHistory : Scale -> History -> Html Msg
viewHistory scale history =
  let
    key =
      if scale.minor then
        (scale.root + 3) % 12
      else
        scale.root
  in
    History.view "pane" key history
