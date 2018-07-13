module Main exposing (..)

import AudioChange
import AudioTime
import Buffet exposing (Buffet, LensChange)
import ChordsInKey
import Circle
import CustomEvents exposing (onChange)
import Highlight exposing (Highlight)
import History exposing (History)
import IdChord exposing (IdChord)
import LowestNote
import Pane exposing (Pane)
import Parse exposing (Parse)
import Pitch
import Player exposing (Player)
import PlayStyle exposing (PlayStyle)
import Ports
import Radio
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Song
import Storage exposing (Storage)
import StrumPattern exposing (StrumPattern)
import Substring exposing (Substring)
import Swatch
import Theater

import AnimationFrame
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, textarea, input
  , select, option, label
  )
import Html.Attributes as Attributes exposing
  ( attribute, href, style, id, type_, value, selected, checked
  , disabled, placeholder
  )
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Lazy
import Navigation exposing (Location)
import Task
import Url

type alias Flags =
  { storage : String
  , canStore : Bool
  }

main : Program Flags Model Msg
main =
  Navigation.programWithFlags
    UrlChanged
    { init = init
    , view = Html.Lazy.lazy view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { title : String
  , bpm : Maybe Float
  , lowestNote : Maybe Int
  , canStore : Bool
  , shouldStore : Bool
  , parse : Parse
  , memory : Maybe Backup
  , saved : Bool
  , buffet : Buffet
  , storage : Storage
  , playing : Bool
  , player : Player
  , history : History
  }

type alias Backup =
  { code : String
  , action : String
  }

init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
  let
    ( storage, shouldStore ) =
      if flags.storage == "" then
        ( Storage.default, False )
      else
        case Storage.deserialize flags.storage of
          Ok x ->
            ( x, True )
          Err message ->
            always
              ( Storage.default, False )
              (Debug.log message flags.storage)
  in let
    title =
      Maybe.withDefault
        ""
        (Url.hashParamValue "title" location)
  in let
    maybeText = Url.hashParamValue "text" location
  in let
    text = Maybe.withDefault defaultText maybeText
  in let
    parse = Parse.init text
  in
    ( { title = title
      , bpm = Nothing
      , lowestNote = Nothing
      , canStore = flags.canStore
      , shouldStore = shouldStore
      , parse = parse
      , memory = Nothing
      , saved = maybeText /= Nothing
      , buffet = Buffet.init parse.suggestions
      , storage = storage
      , playing = False
      , player = Player.init
      , history = History.init
      }
    , Cmd.batch
        [ Theater.init
            { text = text
            , selectionStart = String.length text
            , selectionEnd = String.length text
            }
        , Theater.focus
        , if flags.canStore then
            Storage.init
          else
            Cmd.none
        , if title == "" then
            Ports.setTitle parse.defaultTitle
          else
            Ports.setTitle title
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
  = SetTitle String
  | Save
  | PreviewBpm String
  | SetBpm String
  | PreviewLowestNote String
  | SetLowestNote String
  | SetTonic String
  | SetMinor Bool
  | TextChanged String
  | UrlChanged Location
  | BuffetMsg Buffet.Msg
  | SetStorage Storage
  | SetStrumInterval String
  | RequestTime
  | CurrentTime Float
  | IdChordMsg IdChord.Msg
  | Play (IdChord, Float)
  | Stop Float
  | Stopped
  | AddLine String
  | SetShouldStore Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetTitle title ->
      ( { model
        | title = title
        }
      , Cmd.batch
          [ clearUrl model.saved
          , if title == "" then
              Ports.setTitle model.parse.defaultTitle
            else
              Ports.setTitle title
          ]
      )

    Save ->
      ( model
      , if model.title == "" then
          Navigation.modifyUrl
            ("#text=" ++ Url.percentEncode model.parse.code)
        else
          Navigation.modifyUrl
            ( String.concat
                [ "#title="
                , Url.percentEncode model.title
                , "&text="
                , Url.percentEncode model.parse.code
                ]
            )
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

    SetMinor minor ->
      let
        oldScale = model.parse.scale
      in let
        scale = { oldScale | minor = minor }
      in
        doAction "scale" (Parse.setScale scale) model

    TextChanged code ->
      if code == model.parse.code then
        ( model, Cmd.none )
      else
        let parse = Parse.update code model.parse in
          ( { model
            | parse = parse
            , memory = Nothing
            , buffet =
                Buffet.update parse.suggestions model.buffet
            }
          , Cmd.batch
              [ clearUrl model.saved
              , updateTitle model.title parse
              ]
          )

    UrlChanged location ->
      let
        title =
          Maybe.withDefault
            ""
            (Url.hashParamValue "title" location)
      in let
        maybeText = Url.hashParamValue "text" location
      in
        if
          model.saved && title == "" && maybeText == Nothing
        then
          ( { model | saved = False }, Cmd.none )
        else if
          not model.saved &&
            title == model.title &&
            maybeText == Just model.parse.code
        then
          ( { model | saved = True }, Cmd.none )
        else
          let
            text = Maybe.withDefault defaultText maybeText
          in let
            parse = Parse.init text
          in
            ( { title = title
              , bpm = Nothing
              , lowestNote = Nothing
              , canStore = model.canStore
              , shouldStore = model.shouldStore
              , parse = parse
              , memory = Nothing
              , saved = maybeText /= Nothing
              , buffet = Buffet.init parse.suggestions
              , storage =
                  if model.shouldStore then
                    model.storage
                  else
                    Storage.default
              , playing = False
              , player = Player.init
              , history = History.init
              }
            , Cmd.batch
                [ if model.playing then
                    Task.perform Stop AudioTime.now
                  else
                    Cmd.none
                , Theater.init
                    { text = text
                    , selectionStart = String.length text
                    , selectionEnd = String.length text
                    }
                , Theater.focus
                , if title == "" then
                    Ports.setTitle parse.defaultTitle
                  else
                    Ports.setTitle title
                ]
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

    SetStorage storage ->
      ( { model | storage = storage }
      , Cmd.none
      )

    SetStrumInterval strumIntervalString ->
      ( case String.toFloat strumIntervalString of
          Ok strumInterval ->
            let storage = model.storage in
              { model
              | storage =
                  { storage
                  | strumInterval = 0.001 * strumInterval
                  }
              }
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
      ( { model | playing = False }
      , Task.perform Stop AudioTime.now
      )

    Play ( idChord, now ) ->
      let
        ( player, sequence ) =
          Maybe.withDefault
            ( model.player, [] )
            (Player.setTime now model.player)
      in let
        lowestNote = model.parse.lowestNote
      in let
        beatInterval = 60 / model.parse.bpm
      in let
        ( newPlayer, changes ) =
          case model.storage.playStyle of
            PlayStyle.Arpeggio ->
              Player.arp beatInterval lowestNote idChord now player
            PlayStyle.StrumPattern ->
              Player.strumPattern
                model.storage.strumPattern
                beatInterval
                lowestNote
                idChord
                now
                player
            PlayStyle.Strum ->
              Player.strum
                model.storage.strumInterval
                lowestNote
                idChord
                now
                player
            PlayStyle.Pad ->
              Player.pad lowestNote idChord now player
      in
        ( { model
          | playing = True
          , player = newPlayer
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

    Stopped ->
      ( { model | playing = False }
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

    SetShouldStore shouldStore ->
      ( { model | shouldStore = shouldStore }
      , Cmd.none
      )

replace : Replacement -> Model -> ( Model, Cmd msg )
replace replacement model =
  let
    code = Replacement.apply replacement model.parse.code
  in let
    parse = Parse.update code model.parse
  in
    ( { model
      | parse = parse
      , memory = Nothing
      , buffet =
          Buffet.update parse.suggestions model.buffet
      }
    , Cmd.batch
        [ Theater.replace replacement
        , Theater.focus
        , clearUrl model.saved
        , updateTitle model.title parse
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
              let parse = Parse.update oldCode model.parse in
                ( { model
                  | parse = parse
                  , memory = Nothing
                  , buffet =
                      Buffet.update parse.suggestions model.buffet
                  }
                , Cmd.batch
                    [ Theater.hardUndo
                    , clearUrl model.saved
                    , updateTitle model.title parse
                    ]
                )
            else
              ( { model | memory = Nothing }, Cmd.none )
      Just replacement ->
        let
          code = Replacement.apply replacement oldCode
        in let
          parse = Parse.update code model.parse
        in
          ( { model
            | parse = parse
            , memory = Just { action = action, code = oldCode }
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
              , updateTitle model.title parse
              ]
          )

clearUrl : Bool -> Cmd msg
clearUrl saved =
  if saved then
    Navigation.newUrl "#"
  else
    Cmd.none

updateTitle : String -> Parse -> Cmd msg
updateTitle title parse =
  if title == "" then
    Ports.setTitle parse.defaultTitle
  else
    Cmd.none

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Ports.text TextChanged
    , if model.playing then
        Ports.stopped (always Stopped)
      else
        Sub.none
    , if Player.willChange model.player then
        AnimationFrame.times (always RequestTime)
      else
        Sub.none
    ]

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
"brand ."
"title ."
"bpm ."
"scale ."
"lowestNote ."
"theater ."
"buffet buffet"
"playStyle playStyle"
"playSettings playSettings"
"song song"
"paneSelector paneSelector"
"paneSettings paneSettings"
"pane pane"
"misc misc"
/ minmax(auto, 60em) 1fr
"""
          )
        ]
    ]
    [ Html.Lazy.lazy2
        viewStorage
        model.shouldStore
        model.storage
    , viewBrand
    , Html.Lazy.lazy3
        viewTitle
        model.parse.defaultTitle
        model.title
        model.saved
    , Html.Lazy.lazy2
        viewBpm
        (hasBackup "bpm" model)
        (Maybe.withDefault model.parse.bpm model.bpm)
    , Html.Lazy.lazy2
        viewScale
        (hasBackup "scale" model)
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
    , Html.Lazy.lazy2 viewPlayStyle model.storage model.playing
    , Html.Lazy.lazy viewPlaySettings model.storage
    , Html.Lazy.lazy2 viewSong model.player model.parse
    , Html.Lazy.lazy2
        viewPaneSelector
        model.parse.scale
        model.storage
    , Html.Lazy.lazy viewPaneSettings model.storage
    , case model.storage.pane of
        Pane.ChordsInKey ->
          Html.map
            IdChordMsg
            ( ChordsInKey.view
                "pane"
                model.storage
                model.parse.scale
                (Player.status model.player)
            )
        Pane.Circle ->
          Html.map
            IdChordMsg
            ( Circle.view
                "pane"
                model.parse.scale.tonic
                (Player.status model.player)
            )
        Pane.History ->
          Html.map
            AddLine
            ( History.view
                "pane"
                model.parse.scale.tonic
                model.storage.shortenSequences
                model.history
                (Player.sequence model.player)
                (Player.sequenceFinished model.player)
            )
    , Html.Lazy.lazy2
        viewMiscSettings
        model.canStore
        model.shouldStore
    ]

hasBackup : String -> Model -> Bool
hasBackup action model =
  case model.memory of
    Nothing ->
      False
    Just backup ->
      backup.action == action

viewStorage : Bool -> Storage -> Html msg
viewStorage shouldStore storage =
  span
    [ id "storage"
    , style [ ( "display", "none" ) ]
    , attribute
        "value"
        ( if shouldStore then
            Storage.serialize storage
          else
            ""
        )
    ]
    []

viewBrand : Html msg
viewBrand =
  span
    [ style
        [ ( "grid-area", "brand" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "space-between" )
        , ( "margin-bottom", "8px" )
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

viewTitle : String -> String -> Bool -> Html Msg
viewTitle defaultTitle title saved =
  span
    [ style
        [ ( "grid-area", "title" )
        ]
    ]
    [ Html.text "Title "
    , input
        [ type_ "text"
        , onInput SetTitle
        , placeholder defaultTitle
        , value title
        ]
        []
    , Html.text " "
    , button
        [ onClick Save
        , disabled saved
        ]
        [ Html.text "Save in URL"
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
    [ Html.text "Key "
    , select
        [ onInput SetTonic
        ]
        (List.map (viewTonicOption scale) (List.range 0 11))
    , Html.text " "
    , Html.map
        SetMinor
        ( Radio.view
            scale.minor
            [ ( Pitch.view 0 scale.tonic ++ " Major"
              , False
              )
            , ( Pitch.view 3 ((scale.tonic - 3) % 12) ++ " Minor"
              , True
              )
            ]
        )
    ]

viewTonicOption : Scale -> Int -> Html Msg
viewTonicOption scale namesake =
  let
    tonic =
      if scale.minor then
        (namesake + 3) % 12
      else
        namesake
  in let
    scaleName =
      if scale.minor then
        Pitch.view 3 namesake ++ " Minor"
      else
        Pitch.view 0 namesake ++ " Major"
  in
    option
      [ value (toString tonic)
      , selected (scale.tonic == tonic)
      ]
      [ Html.text scaleName
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

viewPlayStyle : Storage -> Bool -> Html Msg
viewPlayStyle storage playing =
  span
    [ style
        [ ( "grid-area", "playStyle" )
        , ( "margin-top", "8px" )
        ]
    ]
    [ Html.text "Play chords as\xA0"
    , Html.map
        (\x -> SetStorage { storage | playStyle = x })
        ( Radio.view
            storage.playStyle
            [ ( "Arpeggio", PlayStyle.Arpeggio )
            , ( "Strum pattern", PlayStyle.StrumPattern )
            , ( "Strum", PlayStyle.Strum )
            , ( "Pad", PlayStyle.Pad )
            ]
        )
    , Html.text " "
    , button
        [ onClick (IdChordMsg IdChord.Stop)
        , CustomEvents.onLeftDown (IdChordMsg IdChord.Stop)
        , disabled (not playing)
        ]
        [ span
            [ style
               [ ( "background", "currentcolor" )
               , ( "width", "0.75em" )
               , ( "height", "0.75em" )
               , ( "display", "inline-block" )
               ]
            ]
            []
        , Html.text " Stop"
        ]
    ]

viewPlaySettings : Storage -> Html Msg
viewPlaySettings storage =
  case storage.playStyle of
    PlayStyle.StrumPattern ->
      span
        [ style
            [ ( "grid-area", "playSettings" )
            ]
        ]
        [ Html.text "Strum pattern\xA0"
        , Html.map
            (\x -> SetStorage { storage | strumPattern = x })
            ( Radio.view
                storage.strumPattern
                [ ( "Basic", StrumPattern.Basic )
                , ( "Indie", StrumPattern.Indie )
                , ( "Modern", StrumPattern.Modern )
                ]
            )
        ]
    PlayStyle.Strum ->
      span
        [ style
            [ ( "grid-area", "playSettings" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            ]
        ]
      [ Html.text "Strum interval\xA0"
      , input
          [ type_ "range"
          , onInput SetStrumInterval
          , Attributes.min "10"
          , Attributes.max "90"
          , Attributes.step "20"
          , value (toString (1000 * storage.strumInterval))
          , style
              [ ( "width", "5em" )
              ]
          ]
          []
      , Html.text
          ( String.concat
              [ "\xA0"
              , toString (1000 * storage.strumInterval)
              , "ms between notes"
              ]
          )
      ]
    _ ->
      span
        [ style
            [ ( "grid-area", "playSettings" )
            ]
        ]
        []

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

viewPaneSelector : Scale -> Storage -> Html Msg
viewPaneSelector scale storage =
  let
    scaleName =
      if scale.minor then
        Pitch.view 3 ((scale.tonic - 3) % 12) ++ " Minor"
      else
        Pitch.view 0 scale.tonic ++ " Major"
  in
    span
      [ style
          [ ( "grid-area", "paneSelector" )
          , ( "margin-top", "8px" )
          ]
      ]
      [ Html.map
          (\x -> SetStorage { storage | pane = x })
          ( Radio.view
              storage.pane
              [ ( "Chords in " ++ scaleName, Pane.ChordsInKey )
              , ( "Circle of fifths", Pane.Circle )
              , ( "Recently played", Pane.History )
              ]
          )
      ]

viewPaneSettings : Storage -> Html Msg
viewPaneSettings storage =
  case storage.pane of
    Pane.ChordsInKey ->
      span
        [ style
            [ ( "grid-area", "paneSettings" )
            ]
        ]
        [ label
            []
            [ input
                [ type_ "checkbox"
                , checked storage.harmonicMinor
                , onCheck
                    (\x -> SetStorage { storage | harmonicMinor = x })
                ]
                []
            , Html.text " Chords from harmonic minor"
            ]
        , Html.text " "
        , label
            []
            [ input
                [ type_ "checkbox"
                , checked storage.extendedChords
                , onCheck
                    (\x -> SetStorage { storage | extendedChords = x })
                ]
                []
            , Html.text " Extended chords"
            ]
        , Html.text " "
        , label
            []
            [ input
                [ type_ "checkbox"
                , checked storage.addedToneChords
                , onCheck
                    (\x -> SetStorage { storage | addedToneChords = x })
                ]
                []
            , Html.text " Added tone chords"
            ]
        ]
    Pane.Circle ->
      span
        [ style
            [ ( "grid-area", "paneSettings" )
            ]
        ]
        []
    Pane.History ->
      span
        [ style
            [ ( "grid-area", "paneSettings" )
            ]
        ]
        [ label
            []
            [ input
                [ type_ "checkbox"
                , checked storage.shortenSequences
                , onCheck
                    (\x -> SetStorage { storage | shortenSequences = x })
                ]
                []
            , Html.text " Show only last 8 chords of each sequence"
            ]
        ]

viewMiscSettings : Bool -> Bool -> Html Msg
viewMiscSettings canStore shouldStore =
  span
    [ style
        [ ( "grid-area", "misc" )
        , ( "margin-top", "8px" )
        ]
    ]
    [ label
        [ style
            [ ( "color"
              , if not canStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not canStore)
            , checked shouldStore
            , onCheck SetShouldStore
            ]
            []
        , Html.text " Remember my settings"
        ]
    ]
