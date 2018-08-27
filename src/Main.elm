module Main exposing (..)

import Arp
import AudioChange
import Buffet exposing (Buffet, LensChange)
import ChordsInKey
import Circle
import CustomEvents exposing
  (onChange, isAudioTimeButton, onClickWithAudioTime)
import FragmentQuery
import Highlight exposing (Highlight)
import History exposing (History)
import IdChord exposing (IdChord)
import Keyboard exposing (Keyboard)
import Lowest
import Midi
import Pane exposing (Pane)
import Parse exposing (Parse)
import Pitch
import Player exposing (Player)
import PlayStatus exposing (PlayStatus)
import PlayStyle exposing (PlayStyle)
import Ports
import Radio
import Replacement exposing (Replacement)
import Scale exposing (Scale)
import Search
import Settings
import Song
import Storage exposing (Storage)
import StrumPattern
import Substring exposing (Substring)
import Swatch
import Theater
import Tour exposing (Tour)
import Warning

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, input, select, option, label
  , canvas
  )
import Html.Attributes as Attributes exposing
  ( attribute, href, style, id, class, type_, value, selected, checked
  , disabled, placeholder
  )
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Lazy
import Json.Encode as Encode
import Task
import Url exposing (Url)
import Url.Parser

type alias Flags =
  { storage : Encode.Value
  , canStore : Bool
  , mac : Bool
  }

main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL

type alias Model =
  { navigationKey : Navigation.Key
  , tour : Tour
  , mac : Bool
  , title : String
  , dragBpm : Maybe Float
  , customBpm : Float
  , dragLowest : Maybe Int
  , customLowest : Int
  , canStore : Bool
  , shouldStore : Bool
  , parse : Parse
  , memory : Maybe Backup
  , saveState : SaveState
  , buffet : Buffet
  , storage : Storage
  , playing : Bool
  , keyboard : Keyboard
  , playStatus : PlayStatus
  , history : History
  }

type alias Backup =
  { code : String
  , action : String
  }

type SaveState
  = Unsaved
  | Saving
  | Saved

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
  let
    maybeStorage =
      case Storage.decode flags.storage of
        Ok x ->
          x
        Err message ->
          always
            Nothing
            (Debug.log message flags.storage)
  in
  let
    storage =
      Maybe.withDefault Storage.default maybeStorage
  in
  let
    title =
      Maybe.withDefault
        ""
        (FragmentQuery.decode "title" url)
    code =
      Maybe.withDefault
        ( if storage.startEmpty then
            ""
          else
            defaultCode
        )
        (FragmentQuery.decode "text" url)
  in
  let
    parse = Parse.init code
  in
    ( { navigationKey = navigationKey
      , tour = Tour.init
      , mac = flags.mac
      , title = title
      , dragBpm = Nothing
      , customBpm = 120
      , dragLowest = Nothing
      , customLowest = 9
      , canStore = flags.canStore
      , shouldStore = maybeStorage /= Nothing
      , parse = parse
      , memory = Nothing
      , saveState = Saved
      , buffet = Buffet.init parse.suggestions
      , storage = storage
      , playing = False
      , keyboard = Keyboard.init
      , playStatus = PlayStatus.Cleared
      , history = History.init
      }
    , Cmd.batch
        [ Ports.setVolume (toFloat storage.volume / 30)
        , Ports.initMeter ()
        , Theater.init
            { text = code
            , selectionStart = String.length code
            , selectionEnd = String.length code
            }
        , Theater.focus
        , Ports.initHarp ()
        ]
    )

defaultCode : String
defaultCode =
  """// Type chord names in this box
// to create play buttons below
C G  Am F
_ G7
"""

-- UPDATE

type Msg
  = NoOp
  | SetTour Tour
  | SetPageNumber String
  | CloseTour
  | SetTitle String
  | Save
  | Download
  | DragBpm String
  | SetBpm String
  | UseDefaultBpm Bool
  | SetTonic String
  | SetMinor Bool
  | DragLowest String
  | SetLowest String
  | UseDefaultLowest Bool
  | TextChanged String
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url
  | BuffetMsg Buffet.Msg
  | SetStorage Storage
  | CurrentTime Float
  | IdChordMsg IdChord.Msg
  | Playing Bool
  | KeyboardMsg Keyboard.Msg
  | AddLine String
  | SetShouldStore Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    SetTour tour ->
      ( { model | tour = tour }
      , if tour.visible then
          Cmd.batch
            [ Tour.scrollIntoView tour.pageNumber
            , if not model.tour.visible then
                Task.attempt
                  (always NoOp)
                  (Dom.focus "tourNext")
              else
                Cmd.none
            ]
        else
          Cmd.none
      )

    SetPageNumber pageNumberString ->
      case String.toInt pageNumberString of
        Just pageNumber ->
          let tour = model.tour in
            ( { model
              | tour = { tour | pageNumber = pageNumber}
              }
            , Cmd.none
            )
        Nothing ->
          Debug.todo
            ("Main.update: Bad page number: " ++ pageNumberString)

    CloseTour ->
      ( let tour = model.tour in
          { model
          | tour = { tour | visible = False }
          }
      , Cmd.none
      )

    SetTitle title ->
      ( { model
        | title = title
        , saveState = Unsaved
        }
      , Navigation.pushUrl model.navigationKey "#"
      )

    Save ->
      ( { model | saveState = Saving }
      , let
          fragmentPairs =
            if model.title == "" then
              [ ( "text", model.parse.code )
              ]
            else
              [ ( "title", model.title )
              , ( "text", model.parse.code )
              ]
        in
          Navigation.replaceUrl
            model.navigationKey
            (FragmentQuery.encode fragmentPairs)
      )

    Download ->
      ( model
      , Ports.download
          { name =
              if model.title == "" then
                model.parse.defaultTitle ++ ".mid"
              else
                model.title ++ ".mid"
          , base16 =
              Midi.fromChords
                (Maybe.withDefault 120 model.parse.bpm)
                (Lowest.toPitch model.parse.scale.tonic model.parse.lowest)
                (Parse.chords model.parse)
          }
      )

    DragBpm bpmString ->
      ( case String.toFloat bpmString of
          Just bpm ->
            { model | dragBpm = Just bpm }
          Nothing ->
            Debug.todo
              ("Main.update: Bad BPM while dragging: " ++ bpmString)
      , Cmd.none
      )

    SetBpm bpmString ->
      case String.toFloat bpmString of
        Just bpm ->
          doAction
            "bpm"
            (Parse.setBpm (Just bpm))
            { model | dragBpm = Nothing }
        Nothing ->
          Debug.todo
            ("Main.update: Bad BPM: " ++ bpmString)

    UseDefaultBpm True ->
      doAction
        "bpm"
        (Parse.setBpm Nothing)
        { model
        | customBpm =
            Maybe.withDefault model.customBpm model.parse.bpm
        }

    UseDefaultBpm False ->
      doAction
        "bpm"
        (Parse.setBpm (Just model.customBpm))
        model

    SetTonic tonicString ->
      case String.toInt tonicString of
        Just tonic ->
          let
            oldScale = model.parse.scale
          in
          let
            scale = { oldScale | tonic = tonic }
          in
            doAction "scale" (Parse.setScale scale) model
        Nothing ->
          Debug.todo
            ("Main.update: Bad tonic: " ++ tonicString)

    SetMinor minor ->
      let
        oldScale = model.parse.scale
      in
      let
        scale = { oldScale | minor = minor }
      in
        doAction "scale" (Parse.setScale scale) model

    DragLowest pitchString ->
      ( case String.toInt pitchString of
          Just pitch ->
            let
              lowest =
                Lowest.fromPitch model.parse.scale.tonic pitch
            in
              { model | dragLowest = Just lowest }
          Nothing ->
            Debug.todo
              ("Main.update: Bad pitch while dragging: " ++ pitchString)
      , Cmd.none
      )

    SetLowest pitchString ->
      case String.toInt pitchString of
        Just pitch ->
          let
            lowest =
              Lowest.fromPitch model.parse.scale.tonic pitch
          in
            doAction
              "lowest"
              (Parse.setLowest (Just lowest))
              { model | dragLowest = Nothing }
        Nothing ->
          Debug.todo
            ("Main.update: Bad pitch: " ++ pitchString)

    UseDefaultLowest True ->
      doAction
        "lowest"
        (Parse.setLowest Nothing)
        { model
        | customLowest =
            Maybe.withDefault model.customLowest model.parse.lowest
        }

    UseDefaultLowest False ->
      doAction
        "lowest"
        (Parse.setLowest (Just model.customLowest))
        model

    TextChanged code ->
      if code == model.parse.code then
        ( model, Cmd.none )
      else
        let parse = Parse.update code model.parse in
          ( { model
            | parse = parse
            , memory = Nothing
            , saveState = Unsaved
            , buffet =
                Buffet.update parse.suggestions model.buffet
            }
          , codeChanged model parse
          )

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Navigation.load (Url.toString url) )
        Browser.External url ->
          ( model, Navigation.load url )

    UrlChanged url ->
      if model.saveState == Saving then
        ( { model | saveState = Saved }
        , Cmd.none
        )
      else if
        model.saveState == Unsaved &&
          Maybe.withDefault "" url.fragment == ""
      then
        ( model, Cmd.none )
      else
        ( model, Navigation.reload )

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
            ( newModel, cmd ) =
              replace
                { old = range
                , new = Swatch.concat suggestion.swatches
                }
                model
          in
            ( newModel
            , Cmd.batch [ cmd, Theater.focus ]
            )

    SetStorage storage ->
      ( { model
        | storage = storage
        , playStatus =
            if
              (storage.playStyle == PlayStyle.Silent) /=
                (model.storage.playStyle == PlayStyle.Silent)
            then
              Keyboard.status
                (storage.playStyle == PlayStyle.Silent)
                model.keyboard
            else
              model.playStatus
        }
      , Cmd.batch
          [ if model.shouldStore then
              Storage.save storage
            else
              Cmd.none
          , if storage.volume /= model.storage.volume then
              Ports.setVolume (toFloat storage.volume / 30)
            else
              Cmd.none
          ]
      )

    CurrentTime now ->
      ( case Player.setTime now model.keyboard.player of
          Nothing ->
            model
          Just newPlayer ->
            setPlayer newPlayer model
      , Cmd.none
      )

    IdChordMsg (IdChord.Play (idChord, now)) ->
      let
        lowestPitch =
          Lowest.toPitch model.parse.scale.tonic model.parse.lowest
        keyboard = model.keyboard
      in
      let
        ( newPlayer, sequence, changes ) =
          case model.storage.playStyle of
            PlayStyle.Strum ->
              Player.strum
                model.storage.strumInterval
                lowestPitch
                idChord
                now
                keyboard.player
            PlayStyle.Pad ->
              Player.pad lowestPitch idChord now keyboard.player
            PlayStyle.Arpeggio ->
              let
                bpm =
                  Maybe.withDefault Arp.defaultBpm model.parse.bpm
              in
                Player.arp
                  (60 / bpm)
                  lowestPitch
                  idChord
                  now
                  keyboard.player
            PlayStyle.StrumPattern ->
              let
                bpm =
                  Maybe.withDefault
                    (StrumPattern.defaultBpm model.storage.strumPattern)
                    model.parse.bpm
              in
                Player.strumPattern
                  model.storage.strumPattern
                  (60 / bpm)
                  lowestPitch
                  idChord
                  now
                  keyboard.player
            PlayStyle.Silent ->
              ( Player.stop now keyboard.player
              , []
              , if keyboard.lastSound == Keyboard.PlayerSound then
                  [ AudioChange.Mute now ]
                else
                  []
              )
      in
      let
        newKeyboard =
          { keyboard
          | player = newPlayer
          , lastSound =
              if model.storage.playStyle /= PlayStyle.Silent then
                Keyboard.PlayerSound
              else if keyboard.lastSound == Keyboard.PlayerSound then
                Keyboard.Clean
              else
                Keyboard.DirtyPluck
          , source =
              if model.storage.playStyle /= PlayStyle.Silent then
                Keyboard.LastPlayed
              else if Keyboard.getId keyboard == Just idChord.id then
                Keyboard.NoChord
              else
                Keyboard.ThisChord idChord
          }
      in
        ( { model
          | playing = model.storage.playStyle /= PlayStyle.Silent
          , keyboard = newKeyboard
          , playStatus =
              Keyboard.status
                (model.storage.playStyle == PlayStyle.Silent)
                newKeyboard
          , history = History.add sequence model.history
          }
        , if List.isEmpty changes then
            Cmd.none
          else
            AudioChange.perform changes
        )

    IdChordMsg (IdChord.Stop now) ->
      ( setPlayer
          (Player.stop now model.keyboard.player)
          { model | playing = False }
      , Cmd.batch
          [ AudioChange.perform [ AudioChange.Mute now ]
          , Ports.clearPlucks ()
          ]
      )

    Playing playing ->
      ( if playing /= model.playing then
          { model | playing = playing }
        else
          model
      , Cmd.none
      )

    KeyboardMsg (Keyboard.AddWord word) ->
      replace
        (Parse.addWord word model.parse)
        model

    KeyboardMsg keyboardMsg ->
      let
        ( newKeyboard, keyboardCmd ) =
          Keyboard.update keyboardMsg model.keyboard
        newPane =
          case keyboardMsg of
            Keyboard.AddPitch _ ->
              Pane.Search
            Keyboard.RemovePitch _ ->
              Pane.Search
            _ ->
              model.storage.pane
      in
      let
        newPlayStatus =
          Keyboard.status
            (model.storage.playStyle == PlayStyle.Silent)
            newKeyboard
        newStorage =
          if newPane /= model.storage.pane then
            let storage = model.storage in
              { storage | pane = newPane }
          else
            model.storage
      in
        ( { model
          | keyboard = newKeyboard
          , playStatus =
              if newPlayStatus /= model.playStatus then
                newPlayStatus
              else
                model.playStatus
          , storage = newStorage
          }
        , Cmd.batch
            [ Cmd.map KeyboardMsg keyboardCmd
            , if model.shouldStore && newPane /= model.storage.pane then
                Storage.save newStorage
              else
                Cmd.none
            ]
        )

    AddLine line ->
      replace
        { old =
            Substring (String.length model.parse.code) ""
        , new =
            if
              String.endsWith "\n" model.parse.code ||
                String.isEmpty model.parse.code
            then
              line ++ "\n"
            else
              "\n" ++ line ++ "\n"
        }
        model

    SetShouldStore shouldStore ->
      ( { model | shouldStore = shouldStore }
      , if shouldStore then
          Storage.save model.storage
        else
          Storage.delete
      )

replace : Replacement -> Model -> ( Model, Cmd msg )
replace replacement model =
  let
    code = Replacement.apply replacement model.parse.code
  in
  let
    parse = Parse.update code model.parse
  in
    ( { model
      | parse = parse
      , memory = Nothing
      , saveState = Unsaved
      , buffet =
          Buffet.update parse.suggestions model.buffet
      }
    , Cmd.batch
        [ Theater.replace replacement
        , codeChanged model parse
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
                  , saveState = Unsaved
                  , buffet =
                      Buffet.update parse.suggestions model.buffet
                  }
                , Cmd.batch
                    [ Theater.hardUndo
                    , codeChanged model parse
                    ]
                )
            else
              ( { model | memory = Nothing }, Cmd.none )
      Just replacement ->
        let
          code = Replacement.apply replacement oldCode
        in
        let
          parse = Parse.update code model.parse
        in
          ( { model
            | parse = parse
            , memory = Just { action = action, code = oldCode }
            , saveState = Unsaved
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
              , codeChanged model parse
              ]
          )

codeChanged : Model -> Parse -> Cmd msg
codeChanged model parse =
  if model.saveState == Saved then
    Navigation.pushUrl model.navigationKey "#"
  else
    Cmd.none

setPlayer : Player -> Model -> Model
setPlayer newPlayer model =
  let
    keyboard = model.keyboard
  in
  let
    newKeyboard =
      { keyboard | player = newPlayer }
  in
  let
    newPlayStatus =
      Keyboard.status
        (model.storage.playStyle == PlayStyle.Silent)
        newKeyboard
  in
    { model
    | keyboard = newKeyboard
    , playStatus =
        if newPlayStatus /= model.playStatus then
          newPlayStatus
        else
          model.playStatus
    }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Ports.text TextChanged
    , Ports.playing Playing
    , if Player.willChange model.keyboard.player then
        Ports.currentTime CurrentTime
      else
        Sub.none
    , if model.tour.visible then
        Ports.escape (always CloseTour)
      else
        Sub.none
    , Ports.harpPlucked (KeyboardMsg << Keyboard.HarpPlucked)
    ]

-- VIEW

view : Model -> Document Msg
view model =
  { title =
      String.concat
        [ if model.saveState == Saved then
            ""
          else
            "*"
        , if model.title == "" then
            model.parse.defaultTitle
          else
            model.title
        , " - Chords"
        ]
  , body =
      [ Html.Lazy.lazy viewGrid model
      ]
  }

viewGrid : Model -> Html Msg
viewGrid model =
  div
    [ style "font-family" "Arial, \"Helvetica Neue\", Helvetica, sans-serif"
    , style "line-height" "1.9"
    , style "white-space" "nowrap"
    , style "position" "relative"
    , style "display" "grid"
    , style
        "grid"
        """
"brand ."
"title ."
"bpm ."
"scale ."
"lowest ."
"theater ."
"buffet buffet"
"playStyle playStyle"
"song song"
"keyboard keyboard"
"paneSelector paneSelector"
"pane pane"
/ minmax(auto, 60em) 1fr
"""
    ]
    [ Html.Lazy.lazy
        Warning.view
        ( model.storage.unsavedWarning &&
            model.saveState /= Saved &&
            model.parse.code /= ""
        )
    , Html.map
        SetTour
        (Html.Lazy.lazy2 Tour.view model.mac model.tour)
    , Html.Lazy.lazy viewBrand model.tour
    , Html.Lazy.lazy3
        viewTitle
        model.parse.defaultTitle
        model.title
        model.saveState
    , viewBpm
        (actionHasBackup "bpm" model)
        ( Maybe.withDefault
            (Maybe.withDefault model.customBpm model.parse.bpm)
            model.dragBpm
        )
        ( case model.storage.playStyle of
            PlayStyle.Arpeggio ->
              Just Arp.defaultBpm
            PlayStyle.StrumPattern ->
              Just
                ( StrumPattern.defaultBpm
                    model.storage.strumPattern
                )
            _ ->
              Nothing
        )
        (model.parse.bpm == Nothing)
    , Html.Lazy.lazy2
        viewScale
        (actionHasBackup "scale" model)
        model.parse.scale
    , Html.Lazy.lazy3
        viewLowest
        (actionHasBackup "lowest" model)
        ( Maybe.withDefault
            (Maybe.withDefault model.customLowest model.parse.lowest)
            model.dragLowest
        )
        model.parse
    , div
        [ id "theater"
        , style "grid-area" "theater"
        , style "font-family" "\"Lucida Console\", Monaco, monospace"
        , style "font-size" "160%"
        , style "line-height" "initial"
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        ]
        []
    , Html.Lazy.lazy2 viewHighlights model.parse model.buffet
    , Html.Lazy.lazy2 viewBuffet model.tour model.buffet
    , Html.Lazy.lazy2 viewPlayStyle model.storage model.playing
    , Html.Lazy.lazy3
        viewSong
        model.tour
        model.playStatus
        model.parse
    , Html.Lazy.lazy3
        viewKeyboard
        model.parse.scale.tonic
        model.parse.lowest
        model.keyboard
    , Html.Lazy.lazy3
        viewPaneSelector
        model.tour
        model.parse.scale
        model.storage
    , span
        [ id "pane"
        , style "grid-area" "pane"
        , style "min-height" "32em"
        ]
        [ case
            Maybe.withDefault
              model.storage.pane
              (Tour.paneShadow model.tour)
          of
            Pane.Search ->
              Html.Lazy.lazy3
                viewSearch
                model.parse.scale.tonic
                model.storage
                model.keyboard
            Pane.ChordsInKey ->
              Html.Lazy.lazy3
                viewChordsInKey
                model.parse.scale
                model.storage
                model.playStatus
            Pane.Circle ->
              Html.Lazy.lazy2
                viewCircle
                model.parse.scale.tonic
                model.playStatus
            Pane.History ->
              Html.map
                interpretHistoryMsg
                ( History.view
                    model.parse.scale.tonic
                    model.storage
                    model.history
                    (Player.sequence model.keyboard.player)
                    (Player.sequenceFinished model.keyboard.player)
                )
            Pane.Settings ->
              Html.Lazy.lazy3
                viewSettings
                model.canStore
                model.shouldStore
                model.storage
        ]
    ]

actionHasBackup : String -> Model -> Bool
actionHasBackup action model =
  case model.memory of
    Nothing ->
      False
    Just backup ->
      backup.action == action

viewBrand : Tour -> Html Msg
viewBrand tour =
  span
    [ id "brand"
    , style "grid-area" "brand"
    , style "display" "flex"
    , style "align-items" "center"
    , style "margin-bottom" "8px"
    ]
    ( List.concat
        [ [ span
              [ style "font-size" "150%"
              , style "line-height" "initial"
              ]
              [ Html.text "Chord progression editor\u{00A0}"
              ]
          ]
        , if tour.visible then
            [ select
                [ onInput SetPageNumber
                ]
                (Tour.viewPageOptions tour.pageNumber)
            , Html.text "\u{00A0}"
            , button
                [ class "button"
                , onClick CloseTour
                ]
                [ Html.text "Close tour"
                ]
            ]
          else
            [ button
                [ class "button"
                , onClick (SetTour { tour | visible = True })
                ]
                [ if tour.pageNumber == 1 then
                    Html.text "Start tour"
                  else
                    Html.text "Resume tour"
                ]
            ]
        , [ span
              [ style "flex" "1"
              ]
              []
          , a
              [ href "https://github.com/evanshort73/chords"
              ]
              [ Html.text "View on GitHub"
              ]
          ]
        ]
    )

viewTitle : String -> String -> SaveState -> Html Msg
viewTitle defaultTitle title saveState =
  span
    [ id "title"
    , style "grid-area" "title"
    ]
    [ Html.text "Title "
    , input
        [ class "textInput"
        , type_ "text"
        , onInput SetTitle
        , placeholder defaultTitle
        , value title
        ]
        []
    , Html.text " "
    , button
        [ class "button"
        , onClick Save
        , disabled (saveState /= Unsaved)
        ]
        [ Html.text "Save in URL"
        ]
    , Html.text " "
    , button
        [ class "button"
        , onClick Download
        ]
        [ Html.text "Download as MIDI"
        ]
    ]

viewBpm : Bool -> Float -> Maybe Float -> Bool -> Html Msg
viewBpm hasBackup bpm maybeDefaultBpm useDefault =
  span
    [ id "bpm"
    , style "grid-area" "bpm"
    , yellowIf hasBackup
    , style "display" "flex"
    , style "align-items" "center"
    ]
    [ span []
        [ Html.text "Tempo\u{00A0}" ]
    , input
        [ type_ "range"
        , class "range"
        , disabled useDefault
        , onInput DragBpm
        , onChange SetBpm
        , value (String.fromFloat bpm)
        , Attributes.size 3
        , Attributes.min "60"
        , Attributes.max "140"
        , Attributes.step "5"
        , style "width" "9.5em"
        ]
        []
    , span
        [ style "min-width" "15ch"
        , style
            "color"
            ( if useDefault then
                "GrayText"
              else
                ""
            )
        ]
        [ Html.text ("\u{00A0}" ++ String.fromFloat bpm ++ " BPM") ]
    , Html.text "\u{00A0}"
    , label
        [ class "checkboxLabel"
        ]
        [ input
            [ type_ "checkbox"
            , checked useDefault
            , onCheck UseDefaultBpm
            ]
            []
        , Html.text
            ( case ( maybeDefaultBpm, useDefault ) of
                ( Just defaultBpm, True ) ->
                  String.concat
                    [ " Default ("
                    , String.fromFloat defaultBpm
                    , " BPM)"
                    ]
                _ ->
                  " Default"
            )
        ]
    ]

viewScale : Bool -> Scale -> Html Msg
viewScale hasBackup scale =
  span
    [ id "scale"
    , style "grid-area" "scale"
    , yellowIf hasBackup
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
            False
            scale.minor
            [ ( Pitch.view 0 scale.tonic ++ " Major"
              , False
              )
            , ( Pitch.view 3 (modBy 12 (scale.tonic - 3)) ++ " Minor"
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
        modBy 12 (namesake + 3)
      else
        namesake
    scaleName =
      if scale.minor then
        Pitch.view 3 namesake ++ " Minor"
      else
        Pitch.view 0 namesake ++ " Major"
  in
    option
      [ value (String.fromInt tonic)
      , selected (scale.tonic == tonic)
      ]
      [ Html.text scaleName
      ]

viewLowest : Bool -> Int -> Parse -> Html Msg
viewLowest hasBackup lowest parse =
  span
    [ id "lowest"
    , style "grid-area" "lowest"
    , yellowIf hasBackup
    , style "display" "flex"
    , style "align-items" "center"
    ]
    [ span []
        [ Html.text "Lowest scale degree\u{00A0}" ]
    , input
        [ type_ "range"
        , class "range"
        , disabled (parse.lowest == Nothing)
        , onInput DragLowest
        , onChange SetLowest
        , value
            ( String.fromInt
                (Lowest.toPitch parse.scale.tonic (Just lowest))
            )
        , Attributes.min (String.fromInt Lowest.rangeStart)
        , Attributes.max (String.fromInt (Lowest.rangeStart + 11))
        , style "width" "10em"
        ]
        []
    , span
        [ style "min-width" "9ch"
        , style
            "color"
            ( if parse.lowest == Nothing then
                "GrayText"
              else
                ""
            )
        ]
        ( Html.text "\u{00A0}" :: Lowest.view parse.scale lowest )
    , Html.text "\u{00A0}"
    , label
        [ class "checkboxLabel"
        ]
        ( (::)
            ( input
                [ type_ "checkbox"
                , checked (parse.lowest == Nothing)
                , onCheck UseDefaultLowest
                ]
                []
            )
            ( if parse.lowest == Nothing then
                List.concat
                  [ [ Html.text " Default (" ]
                  , Lowest.viewDefault parse.scale
                  , [ Html.text ")" ]
                  ]
              else
                [ Html.text " Default" ]
            )
        )
    ]

yellowIf : Bool -> Attribute msg
yellowIf condition =
  if condition then
    style "background" "#fafac0"
  else
    style "background" "inherit"

viewHighlights : Parse -> Buffet -> Html Msg
viewHighlights parse buffet =
  pre
    [ id "highlights"
    , style "grid-area" "theater"
    , style "font-family" "\"Lucida Console\", Monaco, monospace"
    , style "font-size" "160%"
    , style "line-height" "initial"
    , style "padding" "8px"
    , style "border" "2px solid"
    , style "margin" "0"
    , style "white-space" "pre-wrap"
    , style "word-wrap" "break-word"
    , style "color" "transparent"
    ]
    ( List.map
        Swatch.view
        ( Highlight.toSwatches
            (parse.code ++ "\n")
            (Buffet.highlights buffet ++ parse.highlights)
        )
    )

viewBuffet : Tour -> Buffet -> Html Msg
viewBuffet tour buffet =
  Html.map
    BuffetMsg
    (Buffet.view (Tour.shadowBuffet tour buffet))

viewPlayStyle : Storage -> Bool -> Html Msg
viewPlayStyle storage playing =
  span
    [ id "playStyle"
    , style "grid-area" "playStyle"
    , style "position" "-webkit-sticky"
    , style "position" "sticky"
    , style "top" "0px"
    , style "z-index" "2"
    , style "justify-self" "start"
    , style "margin-left" "-8px"
    , style "padding" "8px"
    , style "background" "white"
    , style "border-bottom-right-radius" "5px"
    , style "box-shadow" "rgba(0, 0, 0, 0.5) 1px 1px 8px -1px"
    , style "min-height" "2.8em"
    ]
    [ span
        [ style "display" "flex"
        , style "align-items" "center"
        ]
        [ Html.text "Play chords as\u{00A0}"
        , Html.map
            (\x -> SetStorage { storage | playStyle = x })
            ( Radio.view
                False
                storage.playStyle
                [ ( "Strum", PlayStyle.Strum )
                , ( "Pad", PlayStyle.Pad )
                , ( "Arpeggio", PlayStyle.Arpeggio )
                , ( "Strum pattern", PlayStyle.StrumPattern )
                , ( "Silent", PlayStyle.Silent )
                ]
            )
        , Html.text "\u{00A0}"
        , button
            [ class "button"
            , isAudioTimeButton True
            , onClickWithAudioTime (IdChordMsg << IdChord.Stop)
            , disabled (not playing)
            ]
            [ span
                [ style "background" "currentcolor"
                , style "width" "0.75em"
                , style "height" "0.75em"
                , style "display" "inline-block"
                ]
                []
            , Html.text " Stop"
            ]
        , Html.text "\u{00A0}Vol.\u{00A0}"
        , span
            [ style "position" "relative"
            ]
            [ input
                [ type_ "range"
                , class "range"
                , onInput (SetStorage << Storage.setVolume storage)
                , value (String.fromInt storage.volume)
                , Attributes.min "0"
                , Attributes.max "30"
                , style "width" "8em"
                , style "display" "block"
                , style "padding-bottom" "0"
                ]
                []
            , canvas
                [ style "position" "absolute"
                , style "left" "6px"
                , style "width" "calc(100% - 12px)"
                , style "top" "100%"
                , style "height" "1em"
                , Attributes.width 100
                , Attributes.height 11
                , id "meter"
                ]
                []
            ]
        , Html.text "\u{00A0}"
        , span
            [ style "min-width" "2ch"
            ]
            [ Html.text (String.fromInt storage.volume)
            ]
        ]
    , viewPlaySettings storage
    ]

viewPlaySettings : Storage -> Html Msg
viewPlaySettings storage =
  case storage.playStyle of
    PlayStyle.Strum ->
      span
        [ style "display" "flex"
        , style "align-items" "center"
        ]
        [ Html.text "Strum interval\u{00A0}"
        , input
            [ type_ "range"
            , class "range"
            , onInput (SetStorage << Storage.setStrumInterval storage)
            , Attributes.min "10"
            , Attributes.max "90"
            , Attributes.step "20"
            , value (String.fromFloat (1000 * storage.strumInterval))
            , style "width" "5em"
            ]
            []
        , Html.text
            ( String.concat
                [ "\u{00A0}"
                , String.fromFloat (1000 * storage.strumInterval)
                , "ms between notes"
                ]
            )
        ]
    PlayStyle.StrumPattern ->
      span
        [ style "display" "block"
        , style "align-items" "center"
        ]
        [ Html.text "Strum pattern\u{00A0}"
        , Html.map
            (\x -> SetStorage { storage | strumPattern = x })
            ( Radio.view
                False
                storage.strumPattern
                [ ( "Basic", StrumPattern.Basic )
                , ( "Indie", StrumPattern.Indie )
                , ( "Modern", StrumPattern.Modern )
                ]
            )
        ]
    _ ->
      span [] []

viewSong : Tour -> PlayStatus -> Parse -> Html Msg
viewSong tour playStatus parse =
  Html.map
    IdChordMsg
    ( Song.view
        "song"
        parse.scale.tonic
        playStatus
        (Tour.padSong tour (Parse.song parse))
    )

viewPaneSelector : Tour -> Scale -> Storage -> Html Msg
viewPaneSelector tour scale storage =
  let
    scaleName =
      if scale.minor then
        Pitch.view 3 (modBy 12 (scale.tonic - 3)) ++ " Minor"
      else
        Pitch.view 0 scale.tonic ++ " Major"
    paneShadow = Tour.paneShadow tour
  in
    span
      [ id "paneSelector"
      , style "grid-area" "paneSelector"
      ]
      [ Html.text "View "
      , Html.map
          (\x -> SetStorage { storage | pane = x })
          ( Radio.view
              (paneShadow /= Nothing)
              (Maybe.withDefault storage.pane paneShadow)
              [ ( "Search results", Pane.Search )
              , ( "Chords in " ++ scaleName, Pane.ChordsInKey )
              , ( "Circle of fifths", Pane.Circle )
              , ( "Recently played", Pane.History )
              , ( "Settings", Pane.Settings )
              ]
          )
      ]

viewSearch : Int -> Storage -> Keyboard -> Html Msg
viewSearch tonic storage keyboard =
  let
    showCustomChord =
      keyboard.source == Keyboard.CustomChord
    playStatus =
      Keyboard.status
        (storage.playStyle == PlayStyle.Silent)
        keyboard
  in
    Html.map
      interpretSearchMsg
      ( Search.view
          tonic
          showCustomChord
          keyboard.customCode
          playStatus
      )

interpretSearchMsg : Search.Msg -> Msg
interpretSearchMsg searchMsg =
  case searchMsg of
    Search.ShowCustomChord blob ->
      KeyboardMsg (Keyboard.ShowCustomChord blob)
    Search.IdChordMsg idChordMsg ->
      IdChordMsg idChordMsg

viewChordsInKey : Scale -> Storage -> PlayStatus -> Html Msg
viewChordsInKey scale storage playStatus =
  Html.map
    interpretChordsInKeyMsg
    (ChordsInKey.view storage scale playStatus)

interpretChordsInKeyMsg : ChordsInKey.Msg -> Msg
interpretChordsInKeyMsg chordsInKeyMsg =
  case chordsInKeyMsg of
    ChordsInKey.SetStorage storage ->
      SetStorage storage
    ChordsInKey.IdChordMsg idChordMsg ->
      IdChordMsg idChordMsg

viewCircle : Int -> PlayStatus -> Html Msg
viewCircle tonic playStatus =
  Html.map
    IdChordMsg
    (Circle.view tonic playStatus)

interpretHistoryMsg : History.Msg -> Msg
interpretHistoryMsg historyMsg =
  case historyMsg of
    History.SetStorage storage ->
      SetStorage storage
    History.AddLine line ->
      AddLine line

viewKeyboard : Int -> Maybe Int -> Keyboard -> Html Msg
viewKeyboard tonic lowest keyboard =
  let
    lowestPitch = Lowest.toPitch tonic lowest
  in
    Html.map
      KeyboardMsg
      (Keyboard.view "keyboard" tonic lowestPitch keyboard)

viewSettings : Bool -> Bool -> Storage -> Html Msg
viewSettings canStore shouldStore storage =
  Html.map
    interpretSettingsMsg
    (Settings.view canStore shouldStore storage)

interpretSettingsMsg : Settings.Msg -> Msg
interpretSettingsMsg settingsMsg =
  case settingsMsg of
    Settings.SetShouldStore shouldStore ->
      SetShouldStore shouldStore
    Settings.SetStorage storage ->
      SetStorage storage
