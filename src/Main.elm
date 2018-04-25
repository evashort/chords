module Main exposing (..)

import AudioChange
import AudioTime
import Bracket
import Buffet exposing (Buffet, LensChange)
import CircleOfFifths
import Highlight exposing (Highlight)
import History exposing (History)
import IdChord exposing (IdChord)
import MainParser
import Player exposing (Player, PlayStatus)
import Replacement exposing (Replacement)
import Song
import Substring exposing (Substring)
import Swatch
import UndoCatcher exposing (UndoCatcher)
import Unit exposing (px, em, ch, percent)

import AnimationFrame
import Array exposing (Array)
import Dom
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, text, textarea, input
  , select, option
  )
import Html.Attributes as Attributes exposing
  (href, style, spellcheck, class, classList, type_, value, selected)
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
  , lnOffset : Int
  , home : Bool
  , chordBox : ChordBox
  , undoHistory : List (MainParser.Model, String)
  }

type PlayStyle
  = ArpeggioStyle
  | StrumStyle
  | PadStyle

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
  in
    ( { player = { openings = [], schedule = [] }
      , history = { sequences = [], current = [] }
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , bpm = 85
      , lnOffset = 0
      , home = True
      , chordBox =
          { catcher = UndoCatcher.fromString text
          , parse = parse
          , buffet =
              Buffet.fromSuggestions (MainParser.suggestions parse)
          }
      , undoHistory = []
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
  | IdChordMsg (IdChord.Msg, Float)
  | SetPlayStyle PlayStyle
  | SetStrumInterval String
  | SetBpm String
  | SetOctave2 String
  | SetLowestNote String
  | SetOldLowestNote
  | SetKey String
  | TextChanged String
  | UrlChanged Location
  | BuffetMsg Buffet.Msg
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

    IdChordMsg ( IdChord.Play idChord, now ) ->
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
        lowestNote =
          MainParser.getLowestNote model.chordBox.parse + model.lnOffset
      in let
        ( newPlayer, changes ) =
          case model.playStyle of
            ArpeggioStyle ->
              Player.playArpeggio
                (60 / toFloat model.bpm) lowestNote idChord now player
            StrumStyle ->
              Player.playStrum
                model.strumInterval lowestNote idChord now player
            PadStyle ->
              Player.playPad lowestNote idChord now player
      in
        ( { model | player = newPlayer, history = newHistory }
        , AudioChange.perform changes
        )

    IdChordMsg ( IdChord.Stop, now ) ->
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

    SetOctave2 octave2String ->
      ( case String.toInt octave2String of
          Ok octave2 ->
            let
              ( oldParse, oldUndoHistory, f ) =
                case model.undoHistory of
                  ( p, "octave" ) :: rest ->
                    ( p, rest, UndoCatcher.switch )
                  h ->
                    ( model.chordBox.parse, h, UndoCatcher.replace )
            in let
              oldLowestNote =
                MainParser.getLowestNote oldParse
            in let
              oldOctave2 = getOctave (oldLowestNote + 6)
            in let
              lowestNote =
                oldLowestNote + 12 * (octave2 - oldOctave2)
            in let
              replacement =
                MainParser.setLowestNote lowestNote oldParse
            in
              { model
              | chordBox = mapCatcher (f replacement) model.chordBox
              , undoHistory = ( oldParse, "octave" ) :: oldUndoHistory
              }
          Err _ ->
            model
      , Cmd.none
      )

    SetLowestNote offsetString ->
      ( case String.toInt offsetString of
          Ok offset ->
            { model | lnOffset = offset }
          Err _ ->
            model
      , Cmd.none
      )

    SetOldLowestNote ->
      ( let
          ( oldParse, oldUndoHistory, f ) =
            case model.undoHistory of
              ( p, "lowestNote" ) :: rest ->
                ( p, rest, UndoCatcher.switch )
              h ->
                ( model.chordBox.parse, h, UndoCatcher.replace )
        in let
          lowestNote =
            MainParser.getLowestNote model.chordBox.parse +
              model.lnOffset
        in let
          replacement =
            MainParser.setLowestNote lowestNote oldParse
        in
          { model
          | lnOffset = 0
          , chordBox =
              mapCatcher (f replacement) model.chordBox
          , undoHistory =
              ( oldParse, "lowestNote" ) :: oldUndoHistory
          }
      , Cmd.none
      )

    SetKey keyString ->
      ( case String.toInt keyString of
          Ok key ->
            let
              ( oldParse, oldUndoHistory, f ) =
                case model.undoHistory of
                  ( p, "key" ) :: rest ->
                    ( p, rest, UndoCatcher.switchAll )
                  h ->
                    ( model.chordBox.parse, h, UndoCatcher.replaceAll )
            in let
              oldLowestNote =
                MainParser.getLowestNote oldParse
            in let
              oldKey = MainParser.getKey oldParse
            in let
              offset = (key - oldKey + 5) % 12 - 5
            in let
              lowestNote = oldLowestNote + offset
            in let
              keyReplacement = MainParser.setKey key oldParse
            in let
              lnReplacement =
                MainParser.setLowestNote lowestNote oldParse
            in let
              transposition =
                MainParser.transpose offset oldParse
            in let
              replacements =
                if keyReplacement.old.i < lnReplacement.old.i then
                  keyReplacement :: lnReplacement :: transposition
                else
                  lnReplacement :: keyReplacement :: transposition
            in
              { model
              | chordBox =
                  mapCatcher (f replacements) model.chordBox
              , undoHistory = ( oldParse, "key" ) :: oldUndoHistory
              }
          Err _ ->
            model
      , Cmd.none
      )

    TextChanged newText ->
      ( { model
        | home = False
        , chordBox =
            mapCatcher (UndoCatcher.update newText) model.chordBox
        , undoHistory = []
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
                  ( UndoCatcher.replace
                      ( Replacement
                          ( Substring
                              0
                              model.chordBox.catcher.frame.text
                          )
                          newText
                      )
                  )
                  model.chordBox
            , undoHistory = []
            }
          , Task.attempt (always NoOp) (Dom.focus "catcher")
          )
        else
          ( model, Cmd.none )

    BuffetMsg (Buffet.LensesChanged lensChange) ->
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

    BuffetMsg (Buffet.Replace suggestion) ->
      ( case suggestion.ranges of
          [] ->
            model
          range :: _ ->
            { model
            | chordBox =
                mapCatcher
                  ( UndoCatcher.replace
                      ( Replacement
                          range
                          (Swatch.concat suggestion.swatches)
                      )
                  )
                  model.chordBox
            , undoHistory = []
            }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

    Undo ->
      ( { model
        | chordBox = mapCatcher UndoCatcher.undo model.chordBox
        , undoHistory = List.drop 1 model.undoHistory
        }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

    Redo ->
      ( { model
        | chordBox = mapCatcher UndoCatcher.redo model.chordBox
        , undoHistory = []
        }
      , Task.attempt (always NoOp) (Dom.focus "catcher")
      )

mapCatcher : (UndoCatcher -> UndoCatcher) -> ChordBox -> ChordBox
mapCatcher f chordBox =
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
          (MainParser.suggestions parse)
          chordBox.buffet
    }

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
        , Html.Lazy.lazy viewKey (MainParser.getKey model.chordBox.parse)
        , span
            [ style
                [ ( "grid-area", "o1" )
                , ( "display", "flex" )
                , ( "justify-content", "space-between" )
                ]
            ]
            [ Html.text "Octave\xA0"
            , span
                []
                [ Html.text
                    ( toString
                        ( getOctave
                            ( MainParser.getLowestNote
                                model.chordBox.parse -
                              6
                            )
                        )
                    )
                ]
            ]
        , input
            [ type_ "number"
            , value
                ( toString
                    ( getOctave
                        ( MainParser.getLowestNote
                            model.chordBox.parse +
                          6
                        )
                    )
                )
            , Attributes.min "-1"
            , Attributes.max "5"
            , onInput SetOctave2
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
        , Html.Lazy.lazy
            viewLowestNote model.lnOffset
        , let
            oldLowestNote =
              MainParser.getLowestNote model.chordBox.parse
          in let
            lowestNote = oldLowestNote + model.lnOffset
          in
            Html.Lazy.lazy2
              viewBrackets oldLowestNote lowestNote
        , let
            oldLowestNote =
              MainParser.getLowestNote model.chordBox.parse
          in let
            lowestNote = oldLowestNote + model.lnOffset
          in
            Html.Lazy.lazy2
              viewLowestNoteText oldLowestNote lowestNote
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
        , Html.Lazy.lazy viewChordBox model.chordBox
        , Html.map
            BuffetMsg
            (Html.Lazy.lazy Buffet.view model.chordBox.buffet)
        ]
    , Html.Lazy.lazy2
        viewSong model.player model.chordBox.parse
    , Html.Lazy.lazy2
        viewCircleOfFifths
        (MainParser.getKey model.chordBox.parse)
        model.player
    , Html.Lazy.lazy2
        History.view
        (MainParser.getKey model.chordBox.parse)
        model.history.sequences
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

getOctave : Int -> Int
getOctave pitch =
  (pitch - pitch % 12) // 12 - 2

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
                , Attributes.min "0"
                , Attributes.max "100"
                , Attributes.step "20"
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
        , Attributes.size 3
        , Attributes.min "60"
        , Attributes.max "140"
        , Attributes.step "5"
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

viewBrackets : Int -> Int -> Html msg
viewBrackets oldLowestNote lowestNote =
  Bracket.view
    "ln2"
    (em (-0.6))
    (Unit.sum [ percent 50, px 11 ])
    (px 6.5)
    (ch 1)
    oldLowestNote
    lowestNote

viewLowestNote : Int -> Html Msg
viewLowestNote offset =
  input
    [ type_ "range"
    , onInput SetLowestNote
    , Attributes.min "-6"
    , Attributes.max "6"
    , value (toString offset)
    , style
        [ ( "grid-area", "ln2" )
        , ( "width", "auto" )
        , ( "min-width", "7em" )
        ]
    ]
    []

viewLowestNoteText : Int -> Int -> Html Msg
viewLowestNoteText oldLowestNote lowestNote =
  let
    octaveOffset = lowestNote % 12
  in let
    octave = (lowestNote - octaveOffset) // 12 - 2
  in let
    lowestNoteText =
      case Array.get octaveOffset flatNames of
        Nothing -> "error"
        Just flatName -> "\xA0" ++ flatName ++ toString octave
  in
    span
      [ style [ ( "grid-area", "ln3" ) ]
      ]
      ( if lowestNote == oldLowestNote then
          [ span
              [ style
                  [ ( "display", "inline-block" )
                  , ( "width", "5ch" )
                  ]
              ]
              [ Html.text lowestNoteText ]
          ]
        else
          [ span
              [ style
                  [ ( "display", "inline-block" )
                  , ( "width", "5ch" )
                  ]
              ]
              [ Html.text lowestNoteText ]
          , button
              [ onClick SetOldLowestNote ]
              [ Html.text "OK" ]
          , button
              [ onClick (SetLowestNote "0") ]
              [ Html.text "Cancel" ]
          ]
      )

flatNames : Array String
flatNames =
  Array.fromList
    [ "C", "D♭", "D", "E♭", "E", "F", "G♭", "G", "A♭", "A", "B♭", "B" ]

viewChordBox : ChordBox -> Html Msg
viewChordBox chordBox =
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
            , MainParser.view chordBox.parse
            , [ Highlight
                  "#000000"
                  "#ffffff"
                  (Substring 0 (chordBox.catcher.frame.text ++ "\n"))
              ]
            ]
        )
    )

viewCircleOfFifths : Int -> Player -> Html Msg
viewCircleOfFifths key player =
  Html.map
    (needsTimeAndTag IdChordMsg)
    (CircleOfFifths.view key (Player.playStatus player))

viewSong : Player -> MainParser.Model -> Html Msg
viewSong player parse =
  Html.map
    (needsTimeAndTag IdChordMsg)
    ( Song.view
        (MainParser.getKey parse)
        (Player.playStatus player)
        (MainParser.song parse)
    )

needsTimeAndTag : ((a, Float) -> Msg) -> a -> Msg
needsTimeAndTag tag x =
  NeedsTime (tag << (,) x)
