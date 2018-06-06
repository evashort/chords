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
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Song
import Substring exposing (Substring)
import Swatch
import Theater

import AnimationFrame
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, text, textarea, input
  , select, option
  )
import Html.Attributes as Attributes exposing
  (href, style, class, classList, id, type_, value, selected, disabled)
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
  , bpm : Maybe Float
  , lowestNote : Maybe Int
  , saved : Bool
  , parse : Parse
  , buffet : Buffet
  , memory : Maybe Backup
  }

type PlayStyle
  = ArpeggioStyle
  | StrumStyle
  | PadStyle

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
    ( { player = { openings = [], schedule = [] }
      , history = { sequences = [], current = [] }
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , bpm = Nothing
      , lowestNote = Nothing
      , saved = maybeText /= Nothing
      , parse = parse
      , buffet = Buffet.init parse.suggestions
      , memory = Nothing
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
  = RequestTime
  | CurrentTime Float
  | PlayStatusMsg PlayStatus.Msg
  | Play (IdChord, Float)
  | Stop Float
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | PreviewBpm String
  | SetBpm String
  | PreviewLowestNote String
  | SetLowestNote String
  | SetKey String
  | TextChanged String
  | UrlChanged Location
  | Save
  | BuffetMsg Buffet.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
          | saved = False
          , parse = parse
          , buffet =
              Buffet.update parse.suggestions model.buffet
          , memory = Nothing
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
                | saved = True
                , parse = parse
                , buffet =
                    Buffet.update parse.suggestions model.buffet
                , memory = Nothing
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
                | saved = False
                , parse = parse
                , buffet =
                    Buffet.update parse.suggestions model.buffet
                , memory = Nothing
                }
            , Cmd.batch
                [ Theater.replace replacement
                , Theater.focus
                , clearUrl model.saved
                ]
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
                  | saved = False
                  , parse = parse
                  , buffet =
                      Buffet.update parse.suggestions model.buffet
                  , memory = Nothing
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
              | saved = False
              , parse = parse
              , buffet =
                  Buffet.update parse.suggestions model.buffet
              , memory = Just { action = action, code = oldCode }
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
        , ( "font-size", "85%" )
        ]
    ]
    [ span
        [ style
            [ ( "position", "relative" )
            , ( "display", "inline-grid" )
            , ( "grid", """
"playStyle ."
"bpm ."
"key ."
"lowestNote ."
"theater save"
"buffet ."
/ 37.5em auto
"""
              )
            , ( "align-items", "center" )
            , ( "line-height", "2.2" )
            , ( "white-space", "nowrap" )
            ]
        ]
        [ Html.Lazy.lazy2
            viewPlayStyle
            model.playStyle
            model.strumInterval
        , Html.Lazy.lazy2
            viewBpm
            (hasBackup "bpm" model)
            (Maybe.withDefault model.parse.bpm model.bpm)
        , Html.Lazy.lazy2
            viewKey
            (hasBackup "key" model)
            model.parse.scale
        , Html.Lazy.lazy2
            viewLowestNote
            (hasBackup "lowestNote" model)
            (Maybe.withDefault model.parse.lowestNote model.lowestNote)
        , div
            [ id "theater"
            , style
                [ ( "grid-area", "theater" )
                , ( "font-family"
                  , "\"Lucida Console\", Monaco, monospace"
                  )
                , ( "font-size", "200%" )
                , ( "line-height", "initial" )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "bottom", "0" )
                ]
            ]
            [ ]
        , Html.Lazy.lazy2 viewHighlights model.parse model.buffet
        , Html.Lazy.lazy viewBuffet model.buffet
        , button
            [ onClick Save
            , disabled model.saved
            , style
                [ ( "grid-area", "save" )
                , ( "align-self", "end" )
                , ( "margin-left", "5px" )
                ]
            ]
            [ Html.text "Save in URL"
            ]
        ]
    , Html.Lazy.lazy2 viewSong model.player model.parse
    , Html.Lazy.lazy2 viewCircleOfFifths model.parse.scale model.player
    , Html.Lazy.lazy2
        History.view
        model.parse.scale.root
        model.history.sequences
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

hasBackup : String -> Model -> Bool
hasBackup action model =
  case model.memory of
    Nothing ->
      False
    Just backup ->
      backup.action == action

viewPlayStyle : PlayStyle -> Float -> Html Msg
viewPlayStyle playStyle strumInterval =
  span
    [ style
        [ ( "grid-area", "playStyle" )
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

viewBpm : Bool -> Float -> Html Msg
viewBpm hasBackup bpm =
  span
    [ style
        [ ( "grid-area", "bpm" )
        , yellowIf hasBackup
        , ( "display", "flex" )
        , ( "align-items", "center" )
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

viewSong : Player -> Parse -> Html Msg
viewSong player parse =
  Html.map
    PlayStatusMsg
    ( Song.view
        parse.scale.root
        (Player.status player)
        (Parse.song parse)
    )

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
      (CircleOfFifths.view key (Player.status player))
