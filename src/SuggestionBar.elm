module SuggestionBar exposing (Model, init, Msg(..), update, view)

import Selection
import Substring exposing (Substring)
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
  , highlighted : Maybe Suggestion
  , recentlyCopied : Set String
  , hovered : Maybe Suggestion
  , focused : Maybe Suggestion
  , landingPad : Maybe Substring
  , landingPadSelected : Bool
  , chordBoxFocused : Bool
  }

init : Bool -> Model
init mac =
  { mac = mac
  , suggestions = []
  , highlighted = Nothing
  , recentlyCopied = Set.empty
  , hovered = Nothing
  , focused = Nothing
  , landingPad = Nothing
  , landingPadSelected = False
  , chordBoxFocused = True
  }

type Msg
  = SuggestionsChanged (List Suggestion)
  | ReceivedSelection ( Int, Int )
  | ChordBoxFocused Bool
  | SuggestionMsg ( Suggestion, Suggestion.Msg )
  | RemoveCopied String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SuggestionsChanged suggestions ->
      ( { model
        | suggestions = suggestions
        , highlighted = Nothing
        , hovered = Nothing
        , focused = Nothing
        , landingPad = Nothing
        , landingPadSelected = False
        }
      , Cmd.none
      )

    ReceivedSelection selection ->
      ( { model
        | landingPadSelected =
            case model.landingPad of
              Just landingPad ->
                selection == Substring.range landingPad
              Nothing ->
                False
        }
      , Cmd.none
      )

    ChordBoxFocused chordBoxFocused ->
      ( { model | chordBoxFocused = chordBoxFocused }, Cmd.none )

    SuggestionMsg (suggestion, Suggestion.Enter) ->
      ( { model
        | highlighted = Just suggestion
        , hovered = Just suggestion
        }
      , Cmd.none
      )

    SuggestionMsg (suggestion, Suggestion.Leave) ->
      ( { model
        | highlighted = model.focused
        , hovered = Nothing
        }
      , Cmd.none
      )

    SuggestionMsg (suggestion, Suggestion.Focus) ->
      ( { model
        | highlighted = Just suggestion
        , focused = Just suggestion
        }
      , Cmd.none
      )

    SuggestionMsg (suggestion, Suggestion.Blur) ->
      ( { model
        | highlighted = model.hovered
        , focused = Nothing
        }
      , Cmd.none
      )

    SuggestionMsg (suggestion, Suggestion.Copied) ->
      ( { model
        | recentlyCopied = Set.insert suggestion.s model.recentlyCopied
        , landingPad = Just suggestion.firstRange
        }
      , Cmd.batch
          [ Selection.setSelection (Substring.range suggestion.firstRange)
          , Task.perform
              (always (RemoveCopied suggestion.s))
              (Process.sleep (1 * Time.second))
          ]
      )

    RemoveCopied s ->
      ( { model | recentlyCopied = Set.remove s model.recentlyCopied }
      , Cmd.none
      )

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
      if model.focused /= Nothing && model.hovered == Nothing then
        "Space to copy or Shift-Tab to go back"
      else if model.chordBoxFocused then
        if model.landingPadSelected then
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
