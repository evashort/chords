module SuggestionBar exposing
  (Model, init, Msg(..), update, subscriptions, view)

import Selection
import Suggestion exposing (Suggestion)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Process
import Set exposing (Set)
import Task
import Time

type alias Model =
  { mac : Bool
  , suggestions : List Suggestion
  , highlighted : String
  , recentlyCopied : Set String
  , hovered : String
  , focused : String
  , copiedYet : Bool
  , chordBoxFocused : Bool
  , subscribeToSelection : Bool
  , selection : ( Int, Int )
  }

init : Bool -> Model
init mac =
  { mac = mac
  , suggestions = []
  , highlighted = ""
  , recentlyCopied = Set.empty
  , hovered = ""
  , focused = ""
  , copiedYet = False
  , chordBoxFocused = True
  , subscribeToSelection = True
  , selection = ( 0, 0 )
  }

type Msg
  = SuggestionsChanged (List Suggestion)
  | CheckSelection
  | ReceivedSelection ( Int, Int )
  | ChordBoxFocus
  | ChordBoxBlur
  | SuggestionMsg ( Suggestion, Suggestion.Msg )
  | RemoveCopied String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SuggestionsChanged suggestions ->
      ( { model
        | suggestions = suggestions
        , highlighted = ""
        , hovered = ""
        , focused = ""
        , copiedYet = False
        }
      , Cmd.none
      )
    CheckSelection ->
      ( model, Selection.checkSelection () )
    ReceivedSelection selection ->
      ( { model
        | selection = selection
        , subscribeToSelection = model.chordBoxFocused
        }
      , Cmd.none
      )
    ChordBoxFocus ->
      ( { model
        | chordBoxFocused = True
        , subscribeToSelection = True
        }
      , Selection.checkSelection ()
      )
    ChordBoxBlur ->
      ( { model | chordBoxFocused = False }, Cmd.none )
    SuggestionMsg (suggestion, Suggestion.Enter) ->
      ( { model
        | highlighted = suggestion.s
        , hovered = suggestion.s
        }
      , Cmd.none
      )
    SuggestionMsg (suggestion, Suggestion.Leave) ->
      ( { model
        | highlighted = model.focused
        , hovered = ""
        }
      , Cmd.none
      )
    SuggestionMsg (suggestion, Suggestion.Focus) ->
      ( { model
        | highlighted = suggestion.s
        , focused = suggestion.s
        }
      , Cmd.none
      )
    SuggestionMsg (suggestion, Suggestion.Blur) ->
      ( { model
        | highlighted = model.hovered
        , focused = ""
        }
      , Cmd.none
      )
    SuggestionMsg (suggestion, Suggestion.Copied) ->
      ( { model
        | recentlyCopied = Set.insert suggestion.s model.recentlyCopied
        , copiedYet = True
        , selection = suggestion.firstRange
        }
      , Cmd.batch
          [ Selection.setSelection suggestion.firstRange
          , Task.perform
              (always (RemoveCopied suggestion.s))
              (Process.sleep (1 * Time.second))
          ]
      )
    RemoveCopied s ->
      ( { model | recentlyCopied = Set.remove s model.recentlyCopied }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.subscribeToSelection then
    Sub.batch
      [ Time.every (1 * Time.second) (always CheckSelection)
      , Selection.receiveSelection ReceivedSelection
      ]
  else
    Selection.receiveSelection ReceivedSelection

view : Model -> Html Msg
view model =
  if List.isEmpty model.suggestions then
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
      ( (List.map (viewSuggestion model.recentlyCopied) model.suggestions) ++
          [ span
              [ style
                  [ ( "line-height", "1.28em" )
                  , ( "min-height", "2.56em" )
                  , ( "margin-left", "2px" )
                  ]
              ]
              [ text (getInstructions model) ]
          ]
      )

getInstructions : Model -> String
getInstructions model =
  case model.suggestions of
    [ suggestion ] ->
      if model.focused /= "" && model.hovered == "" then
        "Space to copy or Shift-Tab to go back"
      else if model.chordBoxFocused then
        if model.copiedYet && model.selection == suggestion.firstRange then
          String.concat
            [ if model.mac then "Cmd" else "Ctrl"
            , "-V to paste over selected text"
            ]
        else
          String.concat
            [ "Keyboard shortcut: Tab and then Space to copy the suggested replacement, then "
            , if model.mac then "Cmd" else "Ctrl"
            , "-V to paste over selected text"
            ]
      else
        ""
    _ ->
      ""

viewSuggestion : Set String -> Suggestion -> Html Msg
viewSuggestion recentlyCopied suggestion =
  Html.map
    (SuggestionMsg << (,) suggestion)
    (Suggestion.view (Set.member suggestion.s recentlyCopied) suggestion)
