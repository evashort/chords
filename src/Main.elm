module Main exposing (..)

import AudioChange
import AudioTime
import Buffet exposing (Buffet, LensChange)
import CircleOfFifths
import CustomEvents exposing (onChange)
import DegreeTable
import Highlight exposing (Highlight)
import History exposing (History)
import IdChord exposing (IdChord)
import LowestNote
import Parse exposing (Parse)
import Pitch
import Player exposing (Player)
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
  = DegreesPane
  | FifthsPane
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
    parse = Parse.init text
  in
    ( { bpm = Nothing
      , lowestNote = Nothing
      , parse = parse
      , memory = Nothing
      , saved = maybeText /= Nothing
      , buffet = Buffet.init parse.suggestions
      , playStyle = ArpeggioStyle
      , strumInterval = 0.04
      , player = Player.init
      , pane = DegreesPane
      , history = History.init
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
  """// Type chord names in this box
// to create play buttons below
C G  Am F
_ G7
"""

-- UPDATE

type Msg
  = PreviewBpm String
  | SetBpm String
  | PreviewLowestNote String
  | SetLowestNote String
  | SetTonic String
  | SetMode String
  | TextChanged String
  | UrlChanged Location
  | Save
  | BuffetMsg Buffet.Msg
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | RequestTime
  | CurrentTime Float
  | IdChordMsg IdChord.Msg
  | Play (IdChord, Float)
  | Stop Float
  | SetPane Pane
  | AddLine String

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

    SetTonic tonicString ->
      case String.toInt tonicString of
        Ok tonic ->
          let
            oldScale = model.parse.scale
          in let
            scale = { oldScale | tonic = tonic }
          in
            doAction "scale" (Parse.setScale scale) model
        Err _ ->
          ( model, Cmd.none )

    SetMode modeString ->
      case
        ( case modeString of
            "Major" -> Just False
            "Minor" -> Just True
            _ -> Nothing
        )
      of
        Just minor ->
          let
            oldScale = model.parse.scale
          in let
            scale = { oldScale | minor = minor }
          in
            doAction "scale" (Parse.setScale scale) model
        Nothing ->
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
          replace
            { old = range
            , new = Swatch.concat suggestion.swatches
            }
            model

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
          Just ( player, sequence ) ->
            { model
            | player = player
            , history = History.add sequence model.history
            }
      , Cmd.none
      )

    IdChordMsg (IdChord.Play idChord) ->
      ( model, Task.perform (Play << (,) idChord) AudioTime.now )

    IdChordMsg IdChord.Stop ->
      ( model, Task.perform Stop AudioTime.now )

    Play ( idChord, now ) ->
      let
        ( player, sequence ) =
          Maybe.withDefault
            ( model.player, [] )
            (Player.setTime now model.player)
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
        ( { model
          | player = newPlayer
          , history = History.add sequence model.history
          }
        , AudioChange.perform changes
        )

    Stop now ->
      let
        ( player, sequence, changes ) =
          Player.stop now model.player
      in
        ( { model
          | player = player
          , history = History.add sequence model.history
          }
        , AudioChange.perform changes
        )

    SetPane pane ->
      ( { model | pane = pane }
      , Cmd.none
      )

    AddLine line ->
      replace
        { old =
            Substring (String.length model.parse.code) ""
        , new =
            if String.endsWith "\n" model.parse.code then
              line ++ "\n"
            else
              "\n" ++ line ++ "\n"
        }
        model

replace : Replacement -> Model -> ( Model, Cmd msg )
replace replacement model =
  ( let
      code = Replacement.apply replacement model.parse.code
    in let
      parse = Parse.update code model.parse
    in
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
"scale ."
"theater save"
"buffet buffet"
"playStyle playStyle"
"song song"
"paneSelector paneSelector"
"pane pane"
/ minmax(auto, 37.5em) 1fr
"""
          )
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
        viewScale
        (hasBackup "scale" model)
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
        DegreesPane ->
          Html.Lazy.lazy2
            viewDegreeTable
            model.parse.scale
            model.player
        FifthsPane ->
          Html.Lazy.lazy2
            viewCircleOfFifths
            model.parse.scale
            model.player
        HistoryPane ->
          Html.Lazy.lazy3
            viewHistory
              model.parse.scale
              model.history
              model.player
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

viewScale : Bool -> Scale -> Html Msg
viewScale hasBackup scale =
  span
    [ style
        [ ( "grid-area", "scale" )
        , yellowIf hasBackup
        ]
    ]
    [ Html.text "Scale "
    , select
        [ onInput SetTonic
        ]
        (List.map (viewTonicOption scale) (List.range 0 11))
    , Html.text " "
    , select
        [ onInput SetMode
        ]
        [ option
            [ value "Major"
            , selected (not scale.minor)
            ]
            [ Html.text
                (Pitch.view 0 scale.tonic ++ " Major")
            ]
        , option
            [ value "Minor"
            , selected scale.minor
            ]
            [ Html.text
                (Pitch.view 3 ((scale.tonic - 3) % 12) ++ " Minor")
            ]
        ]
    ]

viewTonicOption : Scale -> Int -> Html Msg
viewTonicOption scale namesake =
  let
    sharpCount = if scale.minor then 3 else 0
  in let
    tonic =
      if scale.minor then
        (namesake + 3) % 12
      else
        namesake
  in
    option
      [ value (toString tonic)
      , selected (scale.tonic == tonic)
      ]
      [ Html.text (Pitch.view sharpCount namesake)
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
    IdChordMsg
    ( Song.view
        "song"
        parse.scale.tonic
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
            [ ( "Scale degrees", DegreesPane )
            , ( "Circle of fifths", FifthsPane )
            , ( "Recently played", HistoryPane )
            ]
        )
    ]

viewDegreeTable : Scale -> Player -> Html Msg
viewDegreeTable scale player =
  Html.map
    IdChordMsg
    (DegreeTable.view "pane" scale (Player.status player))

viewCircleOfFifths : Scale -> Player -> Html Msg
viewCircleOfFifths scale player =
  Html.map
    IdChordMsg
    ( CircleOfFifths.view
        "pane"
        scale.tonic
        (Player.status player)
    )

viewHistory : Scale -> History -> Player -> Html Msg
viewHistory scale history player =
  Html.map
    AddLine
    ( History.view
        "pane"
        scale.tonic
        history
        (Player.sequence player)
        (Player.sequenceFinished player)
    )
