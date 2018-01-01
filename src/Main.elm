port module Main exposing (..)

import AudioChange exposing (AudioChange(..), Note)
import AudioTime
import BubbleSwatch
import CachedChord
import ChordParser exposing (IdChord)
import CircleOfFifths
import CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)
import Flag exposing (Flag(..))
import Highlight exposing (Highlight)
import History exposing (History)
import MainParser
import Player exposing (Player, PlayStatus)
import Selection
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)

import AnimationFrame
import Array
import Dict exposing (Dict)
import Html exposing
  (Html, a, button, div, pre, span, text, textarea, input, select, option)
import Html.Attributes exposing
  (href, style, spellcheck, id, class, classList, type_, value, selected)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Html.Lazy
import Navigation exposing (Location)
import Process
import Set exposing (Set)
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
  , oldKey : Int
  , home : Bool
  , subscribeToSelection : Bool
  , chordBoxFocused : Bool
  , rangeSets : Dict String (Set ( Int, Int ))
  , selection : Maybe ( Int, Int )
  , chordBox : ChordBox
  , suggestionBar : SuggestionBar
  , suggestionState : SuggestionState
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
  { modifierKey : String
  , text : String
  , parse : MainParser.Model
  , highlightRanges : List Substring
  , focusedSelection : Maybe ( Int, Int )
  }

type alias SuggestionBar =
  { modifierKey : String
  , suggestions : List Suggestion
  , focusedNoBubble : Bool
  }

type alias SuggestionState =
  { lenses : List Suggestion.Lens
  , clipboard : String
  , recentlyCopied : Maybe Suggestion.Id
  , copyCount : Int
  }

init : Bool -> Location -> ( Model, Cmd Msg )
init mac location =
  let
    text = textFromLocation location
  in let
    parse =
      MainParser.init CircleOfFifths.chordCount (Substring 0 text)
  in let
    key = parse.key
  in let
    ( suggestions, rangeSets ) = MainParser.getSuggestions key parse
  in let
    n = String.length text
  in let
    modifierKey = if mac then "⌘" else "Ctrl+"
  in
    ( { player = { openings = [], schedule = [] }
      , history = { sequences = [], current = [] }
      , playStyle = ArpeggioStyle
      , strumInterval = 0.06
      , bpm = 85
      , chordLens =
          { octaveBase = 48
          , key = key
          }
      , oldKey = -1
      , home = True
      , subscribeToSelection = True
      , chordBoxFocused = True
      , rangeSets = rangeSets
      , selection = Nothing
      , chordBox =
          { modifierKey = modifierKey
          , text = text
          , parse = parse
          , highlightRanges = []
          , focusedSelection = Nothing
          }
      , suggestionBar =
          { modifierKey = modifierKey
          , suggestions = suggestions
          , focusedNoBubble = True
          }
      , suggestionState =
          { lenses = []
          , clipboard = ""
          , recentlyCopied = Nothing
          , copyCount = 0
          }
      }
    , Selection.set ( n, n )
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
  | ReceivedSelection ( Int, Int )
  | ChordBoxFocused Bool
  | SuggestionMsg Suggestion.Msg
  | RemoveCopied Int

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
            let
              chordLens = model.chordLens
            in let
              suggestionBar = model.suggestionBar
            in let
              ( suggestions, _ ) =
                MainParser.getSuggestions key model.chordBox.parse
            in
              { model
              | chordLens = { chordLens | key = key }
              , suggestionBar =
                  { suggestionBar | suggestions = suggestions }
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
      ( updateChordBoxText newText { model | home = False }
      , if model.home then
          Navigation.newUrl
            ("#text=" ++ Url.percentEncode newText)
        else
          Navigation.modifyUrl
            ("#text=" ++ Url.percentEncode newText)
      )

    UrlChange location ->
      ( let newText = textFromLocation location in
          if newText /= model.chordBox.text then
            updateChordBoxText newText { model | home = True }
          else
            model
      , Cmd.none
      )

    CheckSelection ->
      ( model, Selection.check )

    ReceivedSelection selection ->
      let
        modelSelection =
          case Dict.get model.suggestionState.clipboard model.rangeSets of
            Just rangeSet ->
              if Set.member selection rangeSet then Just selection
              else Nothing
            Nothing ->
              if
                model.chordLens.key /= model.chordBox.parse.key &&
                  selection == Substring.range model.chordBox.parse.keyRange &&
                    model.suggestionState.clipboard ==
                      String.concat
                        [ "key: "
                        , Flag.codeValue (KeyFlag model.chordLens.key)
                        , if model.chordBox.parse.keyRange.s == "" then "\n"
                          else ""
                        ]
              then
                Just selection
              else
                Nothing
      in
        ( if modelSelection == model.selection then
            model
          else
            (updateFocusedNoBubble << updateFocusedSelection)
              { model | selection = modelSelection }
        , Cmd.none
        )

    ChordBoxFocused chordBoxFocused ->
      ( if chordBoxFocused == model.chordBoxFocused then
          model
        else
          (updateFocusedNoBubble << updateFocusedSelection)
            { model
            | chordBoxFocused = chordBoxFocused
            , subscribeToSelection =
                model.subscribeToSelection || chordBoxFocused
            }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.AddLens lens) ->
      ( let
          suggestionState = model.suggestionState
        in
          updateHighlightRanges
            { model
            | suggestionState =
                { suggestionState
                | lenses =
                    lens ::
                      List.filter
                        ((/=) lens.hover << .hover)
                        suggestionState.lenses
                }
            }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.RemoveLens hover) ->
      ( let
          suggestionState = model.suggestionState
        in
          updateHighlightRanges
            { model
            | suggestionState =
                { suggestionState
                | lenses =
                    List.filter
                      ((/=) hover << .hover)
                      suggestionState.lenses
                }
            }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.Copied ( id, replacement )) ->
      let
        suggestionState = model.suggestionState
      in let
        copyCount = suggestionState.copyCount + 1
      in
        ( { model
          | suggestionState =
              { suggestionState
              | clipboard = replacement
              , recentlyCopied = Just id
              , copyCount = copyCount
              }
          }
        , let
            removeCopied =
              Task.perform
                (always (RemoveCopied copyCount))
                (Process.sleep (1 * Time.second))
          in
            case List.reverse (getRangesById model id) of
              [] ->
                removeCopied
              range :: _ ->
                Cmd.batch
                  [ Selection.set (Substring.range range)
                  , removeCopied
                  ]
        )

    RemoveCopied oldCopyCount ->
      ( let suggestionState = model.suggestionState in
          if oldCopyCount < suggestionState.copyCount then
            model
          else
            { model
            | suggestionState =
                { suggestionState | recentlyCopied = Nothing }
            }
      , Cmd.none
      )

updateFocusedSelection : Model -> Model
updateFocusedSelection model =
  let
    chordBox = model.chordBox
  in let
    focusedSelection =
      if model.chordBoxFocused then model.selection else Nothing
  in
    if chordBox.focusedSelection == focusedSelection then
      model
    else
      { model
      | chordBox =
          { chordBox | focusedSelection = focusedSelection }
      }

updateFocusedNoBubble : Model -> Model
updateFocusedNoBubble model =
  let
    suggestionBar = model.suggestionBar
  in let
    focusedNoBubble =
      model.chordBoxFocused && model.selection == Nothing
  in
    if suggestionBar.focusedNoBubble == focusedNoBubble then
      model
    else
      { model
      | suggestionBar =
          { suggestionBar | focusedNoBubble = focusedNoBubble }
      }

updateChordBoxText : String -> Model -> Model
updateChordBoxText newText model =
  let
    chordBox = model.chordBox
  in let
    suggestionBar = model.suggestionBar
  in let
    suggestionState = model.suggestionState
  in let
    parse = MainParser.update (Substring 0 newText) chordBox.parse
  in let
    ( suggestions, rangeSets ) =
      MainParser.getSuggestions model.chordLens.key parse
  in let
    sameReplacement =
      countSharedReplacements suggestions suggestionBar.suggestions
  in let
    samePosition =
      min (sameReplacement + 1) (List.length suggestions)
  in let
    chordLens = model.chordLens
  in
    updateHighlightRanges
      { model
      | chordLens =
          if
            chordLens.key == chordBox.parse.key &&
              parse.key /= chordBox.parse.key &&
                parse.key /= model.oldKey
          then
            { chordLens | key = parse.key }
          else
            chordLens
      , oldKey =
          if parse.key == chordLens.key then
            if chordBox.parse.key /= parse.key then
              chordBox.parse.key
            else
              model.oldKey
          else
            -1
      , rangeSets = rangeSets
      , selection = Nothing
      , chordBox =
          { chordBox
          | text = newText
          , parse = parse
          , focusedSelection = Nothing
          }
      , suggestionBar =
          { suggestionBar
          | suggestions = suggestions
          , focusedNoBubble = model.chordBoxFocused
          }
      , suggestionState =
          { suggestionState
          | lenses =
              List.filterMap
                (keepFirstN samePosition (List.length suggestions))
                suggestionState.lenses
          , recentlyCopied =
              case suggestionState.recentlyCopied of
                Just (Suggestion.IndexId i) ->
                  if i < sameReplacement then suggestionState.recentlyCopied
                  else Nothing
                _ -> suggestionState.recentlyCopied
          }
      }

countSharedReplacements : List Suggestion -> List Suggestion -> Int
countSharedReplacements xs ys =
  case ( xs, ys ) of
    ( x :: xRest, y :: yRest ) ->
      if Swatch.concat x.swatches == Swatch.concat y.swatches then
        1 + countSharedReplacements xRest yRest
      else
        0
    _ ->
      0

keepFirstN : Int -> Int -> Suggestion.Lens -> Maybe Suggestion.Lens
keepFirstN hoverCount focusCount lens =
  case lens.id of
    Suggestion.StringId _ ->
      Just lens
    Suggestion.IndexId i ->
      if i < (if lens.hover then hoverCount else focusCount) then
        Just lens
      else
        Nothing

updateHighlightRanges : Model -> Model
updateHighlightRanges model =
  let
    chordBox = model.chordBox
  in let
    highlightRanges =
      case model.suggestionState.lenses of
        [] -> []
        lens :: _ -> getRangesById model lens.id
  in
    if highlightRanges == chordBox.highlightRanges then
      model
    else
      { model
      | chordBox = { chordBox | highlightRanges = highlightRanges }
      }

getRangesById : Model -> Suggestion.Id -> List Substring
getRangesById model id =
  case id of
    Suggestion.IndexId i ->
      case List.drop i model.suggestionBar.suggestions of
        suggestion :: _ -> suggestion.ranges
        [] -> []
    Suggestion.StringId "key" ->
      if model.chordLens.key /= model.chordBox.parse.key then
        [ model.chordBox.parse.keyRange ]
      else
        []
    Suggestion.StringId _ ->
      []

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

port focusById : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    ( List.filterMap
        identity
        [ Just (Selection.receive ReceivedSelection)
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
    , viewKey
        (model.chordBox.parse.keyRange.s == "")
        ( model.suggestionState.recentlyCopied ==
            Just (Suggestion.StringId "key")
        )
        model.chordBox.parse.key
        model.chordLens.key
    , Html.Lazy.lazy2 viewChordBox model.chordLens.key model.chordBox
    , Html.Lazy.lazy2
        viewSuggestionBar model.suggestionBar model.suggestionState
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
        [ Html.text "Default octave " ]
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

viewKey : Bool -> Bool -> Int -> Int -> Html Msg
viewKey newline recentlyCopied parsedKey key =
  div
    [ style
        [ ( "line-height", "26px" )
        , ( "margin-bottom", "5px" )
        ]
    ]
    ( List.concat
        [ [ span []
            [ Html.text "Key signature " ]
          , select
              [ onInput SetKey
              , style
                  [ ( "margin-right", "5px" )
                  ]
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
        , if key == parsedKey then
            []
          else
            [ Html.map
                SuggestionMsg
                ( Suggestion.view
                    recentlyCopied
                    (Suggestion.StringId "key")
                    ( [ Swatch "#0000ff" "#ffffff" "key:"
                      , Swatch "#000000" "#ffffff" " "
                      , Swatch "#c00000" "#ffffff" (Flag.codeValue (KeyFlag key))
                      ] ++
                        if newline then
                          [ Swatch "#000000" "#ffffff" "\n" ]
                        else
                          []
                    )
                )
            , button
                [ onClick (SetKey (toString parsedKey))
                , class "pressMe"
                , style
                    [ ( "padding", "0px 3px" )
                    , ( "border-width", "1px" )
                    , ( "border-style", "solid" )
                    , ( "border-radius", "3px" )
                    , ( "font", "inherit" )
                    , ( "line-height", "24px" )
                    , ( "margin-left", "2px" )
                    ]
                ]
                [ Html.text
                    ("× Back to " ++ getFlatName parsedKey)
                ]
            ]
        ]
    )

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
        , value chordBox.text
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
        []
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
      case chordBox.focusedSelection of
        Just ( start, stop ) ->
          { bubbleText =
              chordBox.modifierKey ++
                if start == stop then "V to paste here"
                else "V to replace"
          , fg = ""
          , bg = ""
          , substring = Substring start ""
          } :: grays
        Nothing -> grays
  , MainParser.view key chordBox.parse
  , [ Highlight
        ""
        "#000000"
        "#ffffff"
        (Substring 0 (chordBox.text ++ "\n"))
    ]
  ]

viewSuggestionBar : SuggestionBar -> SuggestionState -> Html Msg
viewSuggestionBar suggestionBar suggestionState =
  if List.isEmpty suggestionBar.suggestions then
    div [] []
  else
    div
      [ style
          [ ( "margin-top", "3px" )
          , ( "width", "500px" )
          , ( "display", "flex" )
          , ( "align-items", "flex-start" )
          ]
      ]
      ( List.concat
          [ List.indexedMap
              ( viewSuggestion
                  ( case suggestionState.recentlyCopied of
                      Just (Suggestion.IndexId i) -> i
                      _ -> -1
                  )

              )
              suggestionBar.suggestions
          , [ span
                [ style
                    [ ( "line-height", "1.28em" )
                    , ( "min-height", "2.56em" )
                    , ( "margin-left", "2px" )
                    ]
                ]
                [ text (getInstructions suggestionBar suggestionState) ]
            ]
          ]
      )

viewSuggestion : Int -> Int -> Suggestion -> Html Msg
viewSuggestion recentlyCopied index suggestion =
  Html.map
    SuggestionMsg
    ( Suggestion.view
        (index == recentlyCopied)
        (Suggestion.IndexId index)
        suggestion.swatches
    )

getInstructions : SuggestionBar -> SuggestionState -> String
getInstructions suggestionBar suggestionState =
  case suggestionBar.suggestions of
    [ suggestion ] ->
      if
        List.member
          (String.concat (List.map lensName suggestionState.lenses))
          [ "hifi", "fi", "fihs" ]
      then
        "Space to copy or Shift-Tab to go back"
      else if
        suggestionBar.focusedNoBubble
      then
        String.concat
          [ "Keyboard shortcut: Tab and then Space to copy the suggested replacement, then "
          , suggestionBar.modifierKey
          , "V to paste over selected text"
          ]
      else
        ""
    _ ->
      ""

lensName : Suggestion.Lens -> String
lensName lens =
  String.concat
    [ if lens.hover then "h" else "f"
    , case lens.id of
        Suggestion.IndexId _ -> "i"
        Suggestion.StringId _ -> "s"
    ]

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
