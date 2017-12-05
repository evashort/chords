port module Main exposing (..)

import ArpeggioPlayer exposing (ArpeggioPlayer)
import AudioChange exposing (Note)
import AudioTime
import CachedChord
import Chord exposing (Chord)
import ChordParser exposing (IdChord)
import CircleOfFifths
import CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)
import Highlight exposing (Highlight)
import MainParser
import PlayStatus exposing (PlayStatus, PlaySegment)
import Selection
import Substring exposing (Substring)
import SuggestionBar

import AnimationFrame
import Array
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
  { arpeggioPlayer : ArpeggioPlayer
  , schedule : List PlaySegment
  , strum : Bool
  , strumInterval : Float
  , octaveBase : Int
  , text : String
  , parse : MainParser.Model
  , home : Bool
  , subscribeToSelection : Bool
  , chordBoxFocused : Bool
  , bubble : Maybe Highlight
  , chordBox : ChordBox
  , suggestionBar : SuggestionBar.Model
  }

type alias ChordBox =
  { highlightRanges : List Substring
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
    ( { arpeggioPlayer = { openings = [] }
      , schedule = []
      , strum = False
      , strumInterval = 0
      , octaveBase = 48
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
  | SetOctave String
  | SetOctaveStart String
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
      ( if PlayStatus.canDropBefore now model.schedule then
          { model | schedule = PlayStatus.dropBefore now model.schedule }
        else
          model
      , Cmd.none
      )

    PlayChord ( chord, id, now ) ->
      let
        root = Chord.get chord 0
      in let
        oRoot =
          if 48 <= root && root < 60 then
            (root - model.octaveBase) % 12 + model.octaveBase
          else
            root
      in let
        oChord =
          if oRoot /= root then
            List.map ((+) (oRoot - root)) chord
          else
            chord
      in let
        ( arpeggioPlayer, cmd, schedule ) =
          if model.strum then
            let
              ( cmd, schedule ) =
                playStrum model.strumInterval oChord id now model.schedule
            in
              ( model.arpeggioPlayer, cmd, schedule )
          else
            ArpeggioPlayer.play oChord id now (60 / 85) model.arpeggioPlayer
      in
        ( { model
          | schedule = schedule
          , arpeggioPlayer = arpeggioPlayer
          }
        , cmd
        )

    SetStrum ( strum, now ) ->
      if strum == model.strum then
        ( model, Cmd.none)
      else
        ( let schedule = { stop = 0, segments = [] } in
            { model
            | schedule = []
            , arpeggioPlayer = { openings = [] }
            , strum = strum
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

    SetOctave octaveString ->
      ( case String.toInt octaveString of
          Ok octave ->
            { model
            | octaveBase =
                12 * (octave + 2) + model.octaveBase % 12
            }
          Err _ ->
            model
      , Cmd.none
      )

    SetOctaveStart octaveOffsetString ->
      ( case String.toInt octaveOffsetString of
          Ok octaveOffset ->
            { model
            | octaveBase =
                model.octaveBase - (model.octaveBase % 12) + octaveOffset
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
  Float -> Chord -> Int -> Float -> List PlaySegment ->
    ( Cmd msg, List PlaySegment )
playStrum strumInterval chord id now schedule =
  ( AudioChange.playNotes
      3
      ( case PlayStatus.dropBefore now schedule of
          [] -> False
          segment :: _ -> segment.status.active /= id
      )
      now
      ( List.map
          (toNote strumInterval chord now)
          (List.range 0 (List.length chord))
      )
  , [ { status = { active = id, next = -1 }, stop = now + 2.25 } ]
  )

toNote : Float -> Chord -> Float -> Int -> Note
toNote strumInterval chord now i =
  let pitch = Chord.get chord i in
    { t = now + strumInterval * toFloat i
    , f = 440 * 2 ^ (toFloat (pitch - 69) / 12)
    }

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
        , if model.schedule /= [] then
            Just (AnimationFrame.times (always (NeedsTime CurrentTime)))
          else
            Nothing
        ]
    )

-- VIEW

view : Model -> Html Msg
view model =
  let playStatus = PlayStatus.current model.schedule in
    div
      [ style
          [ ( "font-family", "Arial, Helvetica, sans-serif" )
          , ( "font-size", "10pt" )
          ]
      ]
      [ Html.Lazy.lazy2 viewPlayStyle model.strum model.strumInterval
      , Html.Lazy.lazy viewOctaveBase model.octaveBase
      , Html.Lazy.lazy3 viewChordBox model.text model.parse model.chordBox
      , Html.Lazy.lazy viewSuggestionBar model.suggestionBar
      , Html.Lazy.lazy2 viewChordArea playStatus model.parse
      , Html.Lazy.lazy viewCircleOfFifths playStatus
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
        [ ( "line-height", "26px" )
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
                  , ( "line-height", "24px" )
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
                  , ( "line-height", "24px" )
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

viewOctaveBase : Int -> Html Msg
viewOctaveBase octaveBase =
  let
    octaveOffset = octaveBase % 12
  in let
    octave = (octaveBase - octaveOffset) // 12 - 2
  in
    div
      [ style
          [ ( "line-height", "26px" )
          , ( "margin-bottom", "5px" )
          ]
      ]
      [ span []
          [ Html.text "Root octave " ]
      , input
          [ type_ "number"
          , onInput SetOctave
          , Html.Attributes.value (toString octave)
          , Html.Attributes.size 2
          , Html.Attributes.min "-2"
          , Html.Attributes.max "6"
          , style
              [ ( "width", "3em" )
              ]
          ]
          []
      , input
          [ type_ "range"
          , onInput SetOctaveStart
          , Html.Attributes.min "0"
          , Html.Attributes.max "11"
          , Html.Attributes.value (toString octaveOffset)
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
              ( String.concat
                  [ getFlatName octaveOffset
                  , toString octave
                  , " to "
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

viewChordArea : PlayStatus -> MainParser.Model -> Html Msg
viewChordArea playStatus parse =
  div
    [ style
        [ ( "font-size", "18pt" )
        , ( "margin-right", "5px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    (List.map (viewLine playStatus) (MainParser.getChords parse))

viewLine : PlayStatus -> List (Maybe IdChord) -> Html Msg
viewLine playStatus line =
  div
    [ style
        [ ( "display", "flex" ) ]
    ]
    (List.map (viewMaybeChord playStatus) line)

viewMaybeChord : PlayStatus -> Maybe IdChord -> Html Msg
viewMaybeChord playStatus maybeChord =
  case maybeChord of
    Just chord ->
      viewChord playStatus chord
    Nothing ->
      viewSpace

viewChord : PlayStatus -> IdChord -> Html Msg
viewChord playStatus chord =
  let
    selected =
      playStatus.active == chord.id || playStatus.next == chord.id
  in let
    play = NeedsTime (PlayChord << (,,) chord.cache.chord chord.id)
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

viewCircleOfFifths : PlayStatus -> Html Msg
viewCircleOfFifths playStatus =
  Html.map msgFromCircleOfFifths (CircleOfFifths.view playStatus)

msgFromCircleOfFifths : CircleOfFifths.Msg -> Msg
msgFromCircleOfFifths msg =
  case msg of
    CircleOfFifths.PlayChord ( chord, id ) ->
      NeedsTime (PlayChord << (,,) chord id)
