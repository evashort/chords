module Main exposing (..)

import Arp
import AudioChange
import AudioTime
import Buffet exposing (Buffet, LensChange)
import Chord
import ChordsInKey
import Circle
import CustomEvents exposing (onChange)
import Highlight exposing (Highlight)
import History exposing (History)
import IdChord exposing (IdChord, PlayStatus)
import Keyboard
import Lowest
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
import StrumPattern
import Substring exposing (Substring)
import Swatch
import Theater
import Tour exposing (Tour)

import AnimationFrame
import Dom
import Html exposing
  ( Html, Attribute, a, button, div, pre, span, textarea, input
  , select, option, label, br
  )
import Html.Attributes as Attributes exposing
  ( attribute, href, style, id, type_, value, selected, checked
  , disabled, placeholder
  )
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Lazy
import Navigation exposing (Location)
import Set
import Task
import Url

type alias Flags =
  { storage : String
  , canStore : Bool
  , mac : Bool
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
  { tour : Tour
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
  , player : Player
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

init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
  let
    maybeStorage =
      if flags.storage == "" then
        Nothing
      else
        case Storage.deserialize flags.storage of
          Ok storage ->
            Just storage
          Err message ->
            always
              Nothing
              (Debug.log message flags.storage)
  in let
    storage =
      Maybe.withDefault Storage.default maybeStorage
  in let
    title =
      Maybe.withDefault
        ""
        (Url.hashParamValue "title" location)
  in let
    code =
      Maybe.withDefault
        ( if storage.startEmpty then
            ""
          else
            defaultCode
        )
        (Url.hashParamValue "text" location)
  in let
    parse = Parse.init code
  in
    ( { tour = Tour.init
      , mac = flags.mac
      , title = title
      , dragBpm = Nothing
      , customBpm = 100
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
      , player = Player.init
      , history = History.init
      }
    , Cmd.batch
        [ Theater.init
            { text = code
            , selectionStart = String.length code
            , selectionEnd = String.length code
            }
        , Theater.focus
        , if title == "" then
            Ports.setTitle parse.defaultTitle
          else
            Ports.setTitle title
        , if flags.canStore then
            Storage.init
          else
            Cmd.none
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
  | DragBpm String
  | SetBpm String
  | UseDefaultBpm Bool
  | SetTonic String
  | SetMinor Bool
  | DragLowest String
  | SetLowest String
  | UseDefaultLowest Bool
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
        Ok pageNumber ->
          let tour = model.tour in
            ( { model
              | tour = { tour | pageNumber = pageNumber}
              }
            , Cmd.none
            )
        Err _ ->
          Debug.crash
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
      , Cmd.batch
          [ Navigation.newUrl "#"
          , if title == "" then
              Ports.setTitle ("*" ++ model.parse.defaultTitle)
            else
              Ports.setTitle ("*" ++ title)
          ]
      )

    Save ->
      ( { model | saveState = Saving }
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

    DragBpm bpmString ->
      ( case String.toFloat bpmString of
          Ok bpm ->
            { model | dragBpm = Just bpm }
          Err _ ->
            Debug.crash
              ("Main.update: Bad BPM while dragging: " ++ bpmString)
      , Cmd.none
      )

    SetBpm bpmString ->
      case String.toFloat bpmString of
        Ok bpm ->
          doAction
            "bpm"
            (Parse.setBpm (Just bpm))
            { model | dragBpm = Nothing }
        Err _ ->
          Debug.crash
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
        Ok tonic ->
          let
            oldScale = model.parse.scale
          in let
            scale = { oldScale | tonic = tonic }
          in
            doAction "scale" (Parse.setScale scale) model
        Err _ ->
          Debug.crash
            ("Main.update: Bad tonic: " ++ tonicString)

    SetMinor minor ->
      let
        oldScale = model.parse.scale
      in let
        scale = { oldScale | minor = minor }
      in
        doAction "scale" (Parse.setScale scale) model

    DragLowest pitchString ->
      ( case String.toInt pitchString of
          Ok pitch ->
            let
              lowest =
                Lowest.fromPitch model.parse.scale.tonic pitch
            in
              { model | dragLowest = Just lowest }
          Err _ ->
            Debug.crash
              ("Main.update: Bad pitch while dragging: " ++ pitchString)
      , Cmd.none
      )

    SetLowest pitchString ->
      case String.toInt pitchString of
        Ok pitch ->
          let
            lowest =
              Lowest.fromPitch model.parse.scale.tonic pitch
          in
            doAction
              "lowest"
              (Parse.setLowest (Just lowest))
              { model | dragLowest = Nothing }
        Err _ ->
          Debug.crash
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

    UrlChanged location ->
      if model.saveState == Saving then
        ( { model | saveState = Saved }
        , if model.title == "" then
            Ports.setTitle model.parse.defaultTitle
          else
            Ports.setTitle model.title
        )
      else if
        model.saveState == Unsaved &&
          location.hash == ""
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
            Debug.crash
              ("Main.update: Bad strum interval: " ++ strumIntervalString)
      , Cmd.none
      )

    RequestTime ->
      ( model, Task.perform CurrentTime AudioTime.now )

    CurrentTime now ->
      ( case Player.setTime now model.player of
          Nothing ->
            model
          Just player ->
            { model | player = player }
      , Cmd.none
      )

    IdChordMsg (IdChord.Play idChord) ->
      if model.storage.playStyle == PlayStyle.Silent then
        ( let
            shouldInit =
              case Player.lastPlayed model.player of
                Nothing ->
                  False
                Just lastPlayed ->
                  idChord.id == lastPlayed.id
          in
            { model
            | playing = False
            , player =
                if shouldInit then
                  Player.init
                else
                  Player.silent idChord
            , history =
                History.add
                  (Player.sequence model.player)
                  model.history
            }
        , if model.playing then
            Task.perform Stop AudioTime.now
          else
            Cmd.none
        )
      else
        ( model
        , Task.perform (Play << (,) idChord) AudioTime.now
        )

    IdChordMsg IdChord.Stop ->
      ( { model | playing = False }
      , Task.perform Stop AudioTime.now
      )

    Play ( idChord, now ) ->
      let
        lowestPitch =
          Lowest.pitch model.parse.scale.tonic model.parse.lowest
      in let
        ( player, sequence, changes ) =
          case model.storage.playStyle of
            PlayStyle.Arpeggio ->
              let
                bpm =
                  Maybe.withDefault Arp.defaultBpm model.parse.bpm
              in
                Player.arp (60 / bpm) lowestPitch idChord now model.player
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
                  model.player
            PlayStyle.Strum ->
              Player.strum
                model.storage.strumInterval
                lowestPitch
                idChord
                now
                model.player
            PlayStyle.Pad ->
              Player.pad lowestPitch idChord now model.player
            PlayStyle.Silent ->
              Debug.crash "Main.update: PlayStyle.Silent got through"
      in
        ( { model
          | playing = True
          , player = player
          , history = History.add sequence model.history
          }
        , AudioChange.perform changes
        )

    Stop now ->
      let
        ( player, changes ) =
          Player.stop now model.player
      in
        ( { model | player = player }
        , AudioChange.perform changes
        )

    Stopped ->
      ( { model | playing = False }
      , Cmd.none
      )

    KeyboardMsg (Keyboard.AddPitch pitch) ->
      ( { model
        | playing = False
        , player =
            Player.silent
              { id = IdChord.count
              , chord =
                  Chord.addPitch
                    ( Lowest.pitch
                        model.parse.scale.tonic
                        model.parse.lowest
                    )
                    pitch
                    ( Maybe.map
                        .chord
                        (Player.lastPlayed model.player)
                    )
              }
        , history =
            History.add
              (Player.sequence model.player)
              model.history
        }
      , if model.playing then
          Task.perform Stop AudioTime.now
        else
          Cmd.none
      )

    KeyboardMsg (Keyboard.RemovePitch pitch) ->
      ( { model
        | playing = False
        , player =
            let
              lowestPitch =
                Lowest.pitch model.parse.scale.tonic model.parse.lowest
            in
              case
                Maybe.andThen
                  (Chord.removePitch lowestPitch pitch << .chord)
                  (Player.lastPlayed model.player)
              of
                Nothing ->
                  Player.init
                Just newChord ->
                  Player.silent
                    { id = IdChord.count
                    , chord = newChord
                    }
        , history =
            History.add
              (Player.sequence model.player)
              model.history
        }
      , if model.playing then
          Task.perform Stop AudioTime.now
        else
          Cmd.none
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
      , saveState = Unsaved
      , buffet =
          Buffet.update parse.suggestions model.buffet
      }
    , Cmd.batch
        [ Theater.replace replacement
        , Theater.focus
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
        in let
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
  Cmd.batch
    [ Navigation.newUrl "#"
    , if model.saveState == Saved then
        if model.title == "" then
          Ports.setTitle ("*" ++ parse.defaultTitle)
        else
          Ports.setTitle ("*" ++ model.title)
      else if
        parse.defaultTitle /= model.parse.defaultTitle
      then
        Ports.setTitle ("*" ++ parse.defaultTitle)
      else
        Cmd.none
    ]

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
    , if model.tour.visible then
        Ports.escape (always CloseTour)
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
"lowest ."
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
    , Html.Lazy.lazy
        viewShouldWarn
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
        (hasBackup "bpm" model)
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
        (hasBackup "scale" model)
        model.parse.scale
    , Html.Lazy.lazy3
        viewLowest
        (hasBackup "lowest" model)
        ( Maybe.withDefault
            (Maybe.withDefault model.customLowest model.parse.lowest)
            model.dragLowest
        )
        model.parse
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
    , Html.Lazy.lazy2 viewBuffet model.tour model.buffet
    , Html.Lazy.lazy2 viewPlayStyle model.storage model.playing
    , Html.Lazy.lazy viewPlaySettings model.storage
    , viewSong model.tour (getPlayStatus model) model.parse
    , Html.Lazy.lazy3
        viewPaneSelector
        model.tour
        model.parse.scale
        model.storage
    , Html.Lazy.lazy2 viewPaneSettings model.tour model.storage
    , case
        Maybe.withDefault
          model.storage.pane
          (Tour.paneShadow model.tour)
      of
        Pane.ChordsInKey ->
          Html.map
            IdChordMsg
            ( ChordsInKey.view
                "pane"
                (Tour.shadowStorage model.tour model.storage)
                model.parse.scale
                (getPlayStatus model)
            )
        Pane.Circle ->
          Html.map
            IdChordMsg
            ( Circle.view
                "pane"
                model.parse.scale.tonic
                (getPlayStatus model)
            )
        Pane.Keyboard ->
          let
            lowestPitch =
              Lowest.pitch model.parse.scale.tonic model.parse.lowest
          in
            Html.Lazy.lazy3
              viewKeyboard
              model.parse.scale.tonic
              lowestPitch
              model.player
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
    , Html.Lazy.lazy3
        viewMiscSettings
        model.canStore
        model.shouldStore
        model.storage
    ]

hasBackup : String -> Model -> Bool
hasBackup action model =
  case model.memory of
    Nothing ->
      False
    Just backup ->
      backup.action == action

getPlayStatus : Model -> PlayStatus
getPlayStatus model =
  if model.storage.playStyle == PlayStyle.Silent then
    Player.silentStatus model.player
  else
    Player.status model.player

viewShouldWarn : Bool -> Html msg
viewShouldWarn shouldWarn =
  span
    [ id "warning"
    , style [ ( "display", "none" ) ]
    , attribute
        "value"
        ( if shouldWarn then
            "true"
          else
            ""
        )
    ]
    []

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

viewBrand : Tour -> Html Msg
viewBrand tour =
  span
    [ style
        [ ( "grid-area", "brand" )
        , ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "margin-bottom", "8px" )
        ]
    ]
    ( List.concat
        [ [ span
              [ style
                  [ ( "font-size", "150%" )
                  , ( "line-height", "initial" )
                  ]
              ]
              [ Html.text "Chord progression editor\xA0"
              ]
          ]
        , if tour.visible then
            [ select
                [ onInput SetPageNumber
                ]
                (Tour.viewPageOptions tour.pageNumber)
            , Html.text "\xA0"
            , button
                [ onClick CloseTour
                ]
                [ Html.text "Close tour"
                ]
            ]
          else
            [ button
                [ onClick (SetTour { tour | visible = True })
                ]
                [ if tour.pageNumber == 1 then
                    Html.text "Start tour"
                  else
                    Html.text "Resume tour"
                ]
            ]
        , [ span
              [ style
                  [ ( "flex", "1" )
                  ]
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
        , disabled (saveState /= Unsaved)
        ]
        [ Html.text "Save in URL"
        ]
    ]

viewBpm : Bool -> Float -> Maybe Float -> Bool -> Html Msg
viewBpm hasBackup bpm maybeDefaultBpm useDefault =
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
        , disabled useDefault
        , onInput DragBpm
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
    , span
        [ style
            [ ( "min-width", "15ch" )
            , ( "color"
              , if useDefault then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ Html.text ("\xA0" ++ toString bpm ++ " BPM") ]
    , Html.text "\xA0"
    , label
        []
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
                    , toString defaultBpm
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
            False
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

viewLowest : Bool -> Int -> Parse -> Html Msg
viewLowest hasBackup lowest parse =
  span
    [ style
        [ ( "grid-area", "lowest" )
        , yellowIf hasBackup
        , ( "display", "flex" )
        , ( "align-items", "center" )
        ]
    ]
    [ span []
        [ Html.text "Lowest scale degree\xA0" ]
    , input
        [ type_ "range"
        , disabled (parse.lowest == Nothing)
        , onInput DragLowest
        , onChange SetLowest
        , value
            ( toString
                (Lowest.pitch parse.scale.tonic (Just lowest))
            )
        , Attributes.min (toString Lowest.rangeStart)
        , Attributes.max (toString (Lowest.rangeStart + 11))
        , style
            [ ( "width", "10em" )
            ]
        ]
        []
    , span
        [ style
            [ ( "min-width", "9ch" )
            , ( "color"
              , if parse.lowest == Nothing then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        ( Html.text "\xA0" :: Lowest.view parse.scale lowest )
    , Html.text "\xA0"
    , label
        []
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

viewBuffet : Tour -> Buffet -> Html Msg
viewBuffet tour buffet =
  Html.map
    BuffetMsg
    (Buffet.view (Tour.shadowBuffet tour buffet))

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
            False
            storage.playStyle
            [ ( "Arpeggio", PlayStyle.Arpeggio )
            , ( "Strum pattern", PlayStyle.StrumPattern )
            , ( "Strum", PlayStyle.Strum )
            , ( "Pad", PlayStyle.Pad )
            , ( "Silent", PlayStyle.Silent )
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
                False
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
        Pitch.view 3 ((scale.tonic - 3) % 12) ++ " Minor"
      else
        Pitch.view 0 scale.tonic ++ " Major"
  in let
    paneShadow = Tour.paneShadow tour
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
              (paneShadow /= Nothing)
              (Maybe.withDefault storage.pane paneShadow)
              [ ( "Chords in " ++ scaleName, Pane.ChordsInKey )
              , ( "Circle of fifths", Pane.Circle )
              , ( "Keyboard", Pane.Keyboard )
              , ( "Recently played", Pane.History )
              ]
          )
      ]

viewPaneSettings : Tour -> Storage -> Html Msg
viewPaneSettings tour storage =
  case Maybe.withDefault storage.pane (Tour.paneShadow tour) of
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
            [ style
                [ ( "color"
                  , if Tour.extendedChords tour then
                      "GrayText"
                    else
                      ""
                  )
                ]
            ]
            [ input
                [ type_ "checkbox"
                , disabled (Tour.extendedChords tour)
                , checked
                    ( storage.extendedChords ||
                        Tour.extendedChords tour
                    )
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
                    ( \x ->
                        SetStorage { storage | shortenSequences = x }
                    )
                ]
                []
            , Html.text " Show only last 8 chords of each sequence"
            ]
        ]
    _ ->
      span
        [ style
            [ ( "grid-area", "paneSettings" )
            ]
        ]
        []

viewKeyboard : Int -> Int -> Player -> Html Msg
viewKeyboard tonic lowestPitch player =
  let
    lastPlayed = Player.lastPlayed player
  in let
    pitchSet =
      Maybe.withDefault
        Set.empty
        ( Maybe.map
            (Chord.toPitchSet lowestPitch << .chord)
            lastPlayed
        )
  in
    Html.map
      KeyboardMsg
      ( Keyboard.view
          "pane"
          tonic
          lowestPitch
          (lowestPitch + 32)
          pitchSet
      )

viewMiscSettings : Bool -> Bool -> Storage -> Html Msg
viewMiscSettings canStore shouldStore storage =
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
    , Html.br [] []
    , label
        [ style
            [ ( "color"
              , if not shouldStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not shouldStore)
            , checked (shouldStore && storage.startEmpty)
            , onCheck (\x -> SetStorage { storage | startEmpty = x })
            ]
            []
        , Html.text " Start with an empty textbox"
        ]
    , Html.br [] []
    , label
        [ style
            [ ( "color"
              , if not shouldStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not shouldStore)
            , checked (shouldStore && storage.unsavedWarning)
            , onCheck (\x -> SetStorage { storage | unsavedWarning = x })
            ]
            []
        , Html.text " Warn me when I close without saving"
        ]
    ]
