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
  , lenses : List Suggestion.Lens
  , clipboard : String
  , recentlyCopied : Maybe Suggestion.Id
  , copyCount : Int
  , landingPadSelected : Bool
  , chordBoxFocused : Bool
  }

init : String -> List Suggestion -> Model
init modifierKey suggestions =
  { modifierKey = modifierKey
  , suggestions = suggestions
  , lenses = []
  , clipboard = ""
  , recentlyCopied = Nothing
  , copyCount = 0
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
          sameReplacement =
            countSharedReplacements suggestions model.suggestions
        in let
          samePosition =
            min (sameReplacement + 1) (List.length suggestions)
        in
          { model
          | suggestions = suggestions
          , lenses =
              List.filterMap
                (keepFirstN samePosition (List.length suggestions))
                model.lenses
          , recentlyCopied =
              case model.recentlyCopied of
                Just (Suggestion.IndexId i) ->
                  if i < sameReplacement then model.recentlyCopied
                  else Nothing
                _ -> model.recentlyCopied
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

    SuggestionMsg (Suggestion.AddLens lens) ->
      ( { model
        | lenses =
            lens ::
              List.filter ((/=) lens.hover << .hover) model.lenses
        }
      , Cmd.none
      )

    SuggestionMsg (Suggestion.RemoveLens hover) ->
      ( { model
        | lenses = List.filter ((/=) hover << .hover) model.lenses
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

getSuggestionById : Model -> Suggestion.Id -> Maybe Suggestion
getSuggestionById model id =
  case id of
    Suggestion.IndexId i ->
      List.head (List.drop i model.suggestions)
    _ ->
      Nothing

highlightRanges : Model -> List Substring
highlightRanges model =
  case model.lenses of
    [] -> []
    lens :: _ ->
      case getSuggestionById model lens.id of
        Nothing -> []
        Just suggestion -> suggestion.ranges

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
      if
        List.member
          (String.concat (List.map lensName model.lenses))
          [ "hifi", "fi", "fihs" ]
      then
        "Space to copy or Shift-Tab to go back"
      else if
        model.chordBoxFocused && not model.landingPadSelected
      then
        String.concat
          [ "Keyboard shortcut: Tab and then Space to copy the suggested replacement, then "
          , model.modifierKey
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

viewSuggestion : Int -> Int -> Suggestion -> Html Msg
viewSuggestion recentlyCopied index suggestion =
  Html.map
    SuggestionMsg
    ( Suggestion.view
        (index == recentlyCopied)
        (Suggestion.IndexId index)
        suggestion
    )
