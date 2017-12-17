module SuggestionBar exposing
  (Model, init, Msg(..), update, highlightRanges, landingPads, view)

import Selection exposing (Selection)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Process
import Task
import Time

type alias Model =
  { modifierKey : String
  , suggestions : List Suggestion
  , highlighted : Maybe Int
  , clipboard : String
  , recentlyCopied : Maybe Int
  , copyCount : Int
  , hovered : Maybe Int
  , focused : Maybe Int
  , landingPadSelected : Bool
  , chordBoxFocused : Bool
  }

init : String -> List Suggestion -> Model
init modifierKey suggestions =
  { modifierKey = modifierKey
  , suggestions = suggestions
  , highlighted = Nothing
  , clipboard = ""
  , recentlyCopied = Nothing
  , copyCount = 0
  , hovered = Nothing
  , focused = Nothing
  , landingPadSelected = False
  , chordBoxFocused = True
  }

type Msg
  = SuggestionsChanged (List Suggestion)
  | LandingPadSelected Bool
  | ChordBoxFocused Bool
  | SuggestionMsg Suggestion.Msg
  | RemoveCopied Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SuggestionsChanged suggestions ->
      ( let
          sameCount =
            countSharedReplacements suggestions model.suggestions
        in
          { model
          | suggestions = suggestions
          , highlighted =
              andThenBelow (sameCount + 1) model.highlighted
          , recentlyCopied =
              andThenBelow sameCount model.recentlyCopied
          , hovered =
              andThenBelow (sameCount + 1) model.hovered
          , focused = Nothing
          }
      , Cmd.none
      )

    LandingPadSelected landingPadSelected ->
      ( if landingPadSelected /= model.landingPadSelected then
          { model | landingPadSelected = landingPadSelected }
        else
          model
      , Cmd.none
      )

    ChordBoxFocused chordBoxFocused ->
      ( { model | chordBoxFocused = chordBoxFocused }, Cmd.none )

    SuggestionMsg (Suggestion.Enter ( _, i )) ->
      ( { model
        | highlighted = Just i
        , hovered = Just i
        }
      , Cmd.none
      )

    SuggestionMsg Suggestion.Leave ->
      ( { model
        | highlighted = model.focused
        , hovered = Nothing
        }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.Focus ( _, i )) ->
      ( { model
        | highlighted = Just i
        , focused = Just i
        }
      , Cmd.none
      )

    SuggestionMsg Suggestion.Blur ->
      ( { model
        | highlighted = model.hovered
        , focused = Nothing
        }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.Copied ( _, i )) ->
      case List.drop i model.suggestions of
        [] ->
          ( model, Cmd.none )
        suggestion :: _ ->
          let copyCount = model.copyCount + 1 in
            ( { model
              | clipboard = suggestion.replacement
              , recentlyCopied = Just i
              , copyCount = copyCount
              }
            , Cmd.batch
                [ Selection.set
                    { start = suggestion.firstRange.i
                    , stop = Substring.stop suggestion.firstRange
                    }
                , Task.perform
                    (always (RemoveCopied copyCount))
                    (Process.sleep (1 * Time.second))
                ]
            )

    RemoveCopied oldCopyCount ->
      ( if oldCopyCount < model.copyCount then model
        else { model | recentlyCopied = Nothing }
      , Cmd.none
      )

countSharedReplacements : List Suggestion -> List Suggestion -> Int
countSharedReplacements xs ys =
  case ( xs, ys ) of
    ( x :: xRest, y :: yRest ) ->
      if x.replacement == y.replacement then
        1 + countSharedReplacements xRest yRest
      else
        0
    _ ->
      0

andThenBelow : Int -> Maybe Int -> Maybe Int
andThenBelow bound x =
  case x of
    Just i -> if i < bound then x else Nothing
    Nothing -> x

highlightRanges : Model -> List Substring
highlightRanges model =
  case model.highlighted of
    Nothing -> []
    Just i ->
      case List.drop i model.suggestions of
        [] -> []
        suggestion :: _ -> suggestion.firstRange :: suggestion.ranges

landingPads : Model -> List Selection
landingPads model =
  List.concatMap
    (landingPadsFromSuggestion model.clipboard)
    model.suggestions

landingPadsFromSuggestion : String -> Suggestion -> List Selection
landingPadsFromSuggestion clipboard suggestion =
  if suggestion.replacement == clipboard then
    List.map
      Selection.fromSubstring
      (suggestion.firstRange :: suggestion.ranges)
  else
    []

view : Int -> Model -> Html Msg
view key model =
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
      ( List.concat
          [ List.indexedMap
              (viewSuggestion key model.recentlyCopied)
              model.suggestions
          , [ span
                [ style
                    [ ( "line-height", "1.28em" )
                    , ( "min-height", "2.56em" )
                    , ( "margin-left", "2px" )
                    ]
                ]
                [ text (getInstructions model) ]
            ]
          ]
      )

getInstructions : Model -> String
getInstructions model =
  case model.suggestions of
    [ suggestion ] ->
      if model.focused /= Nothing && model.hovered == Nothing then
        "Space to copy or Shift-Tab to go back"
      else if model.chordBoxFocused && not model.landingPadSelected then
        String.concat
          [ "Keyboard shortcut: Tab and then Space to copy the suggested replacement, then "
          , model.modifierKey
          , "V to paste over selected text"
          ]
      else
        ""
    _ ->
      ""

viewSuggestion : Int -> Maybe Int -> Int -> Suggestion -> Html Msg
viewSuggestion key recentlyCopied index suggestion =
  Html.map
    SuggestionMsg
    ( Suggestion.view
        key
        (Just index == recentlyCopied)
        "suggestion"
        index
        suggestion
    )
