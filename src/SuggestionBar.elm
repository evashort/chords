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
  , highlighted : Maybe Suggestion.Id
  , clipboard : String
  , recentlyCopied : Maybe Suggestion.Id
  , copyCount : Int
  , hovered : Maybe Suggestion.Id
  , focused : Maybe Suggestion.Id
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
              removeIndexAndAbove (sameCount + 1) model.highlighted
          , recentlyCopied =
              removeIndexAndAbove sameCount model.recentlyCopied
          , hovered =
              removeIndexAndAbove (sameCount + 1) model.hovered
          , focused =
              removeIndexAndAbove (sameCount + 1) model.focused
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

    SuggestionMsg (Suggestion.Enter id) ->
      ( { model
        | highlighted = Just id
        , hovered = Just id
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

    SuggestionMsg (Suggestion.Focus id) ->
      ( { model
        | highlighted = Just id
        , focused = Just id
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

    SuggestionMsg (Suggestion.Copied id) ->
      case getSuggestionById model id of
        Nothing ->
          ( model, Cmd.none )
        Just suggestion ->
          let copyCount = model.copyCount + 1 in
            ( { model
              | clipboard = suggestion.replacement
              , recentlyCopied = Just id
              , copyCount = copyCount
              }
            , let
                removeCopied =
                  Task.perform
                    (always (RemoveCopied copyCount))
                    (Process.sleep (1 * Time.second))
              in
                case List.reverse suggestion.ranges of
                  [] ->
                    removeCopied
                  range :: _ ->
                    Cmd.batch
                      [ Selection.set (Selection.fromSubstring range)
                      , removeCopied
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

removeIndexAndAbove : Int -> Maybe Suggestion.Id -> Maybe Suggestion.Id
removeIndexAndAbove index x =
  case x of
    Just (Suggestion.IndexId i) ->
      if i < index then x else Nothing
    _ ->
      x

getSuggestionById : Model -> Suggestion.Id -> Maybe Suggestion
getSuggestionById model id =
  case id of
    Suggestion.IndexId i ->
      List.head (List.drop i model.suggestions)
    _ ->
      Nothing

highlightRanges : Model -> List Substring
highlightRanges model =
  Maybe.withDefault
    []
    ( Maybe.map
        .ranges
        (Maybe.andThen (getSuggestionById model) model.highlighted)
    )

landingPads : Model -> List Selection
landingPads model =
  List.concatMap
    (landingPadsFromSuggestion model.clipboard)
    model.suggestions

landingPadsFromSuggestion : String -> Suggestion -> List Selection
landingPadsFromSuggestion clipboard suggestion =
  if suggestion.replacement == clipboard then
    List.map Selection.fromSubstring suggestion.ranges
  else
    []

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
      ( List.concat
          [ List.indexedMap
              ( viewSuggestion
                  ( case model.recentlyCopied of
                      Just (Suggestion.IndexId i) -> i
                      _ -> -1
                  )

              )
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

viewSuggestion : Int -> Int -> Suggestion -> Html Msg
viewSuggestion recentlyCopied index suggestion =
  Html.map
    SuggestionMsg
    ( Suggestion.view
        (index == recentlyCopied)
        (Suggestion.IndexId index)
        suggestion
    )
