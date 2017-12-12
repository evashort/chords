module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import ChordParser exposing (IdChord)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)

import Dict exposing (Dict)
import Regex exposing (Regex, HowMany(..), Match)
import Set exposing (Set)

type alias Model =
  { chordModel : ChordParser.Model
  , assignments : List Assignment
  , comments : List Substring
  , indents : List Substring
  }

init : Int -> Substring -> Model
init firstId substring =
  let parseResult = parse substring in
    { chordModel =
        ChordParser.init firstId parseResult.words
    , assignments = parseResult.assignments
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

update : Substring -> Model -> Model
update whole model =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.update parseResult.words model.chordModel
    , assignments = parseResult.assignments
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

view : Int -> Model -> List Highlight
view key model =
  ChordParser.view key model.chordModel ++
    List.map (Highlight "" "#008000" "#ffffff") model.comments ++
      List.map (Highlight "" "#ffffff" "#ff0000") model.indents ++
        List.concatMap viewAssignment model.assignments

viewAssignment : Assignment -> List Highlight
viewAssignment assignment =
  if
    assignment.variable.s /= assignment.cleanVariable ||
      missingSpace assignment
  then
    []
  else
    [ Highlight "" "#0000ff" "#ffffff" assignment.variable ]

getChords : Model -> List (List (Maybe IdChord))
getChords = ChordParser.getChords << .chordModel

getSuggestions : Model -> List Suggestion
getSuggestions model =
  List.sortBy
    (.i << .firstRange)
    ( List.concat
        [ ChordParser.getSuggestions model.chordModel
        , Dict.values (List.foldl addSuggestion Dict.empty model.assignments)
        ]
    )

addSuggestion :
  Assignment -> Dict String Suggestion -> Dict String Suggestion
addSuggestion assignment suggestions =
  if missingSpace assignment then
    let
      replacement =
        String.join
          " "
          [ assignment.cleanVariable
          , Maybe.withDefault "" (Maybe.map .s assignment.value)
          ]
    in
      Dict.update
        replacement
        (updateAssignmentSuggestion assignment replacement)
        suggestions
  else if
    assignment.variable.s /= assignment.cleanVariable
  then
    Dict.update
      assignment.cleanVariable
      (updateVariableSuggestion assignment)
      suggestions
  else
    suggestions

updateAssignmentSuggestion :
  Assignment -> String -> Maybe Suggestion -> Maybe Suggestion
updateAssignmentSuggestion assignment replacement maybeSuggestion =
  Just <|
    case maybeSuggestion of
      Nothing ->
        { replacement = replacement
        , swatchLists =
            let
              swatchList =
                [ Swatch "#0000ff" "#ffffff" assignment.cleanVariable
                , Swatch
                    "#000000"
                    "#ffffff"
                    ( String.concat
                        [ " "
                        , Maybe.withDefault "" (Maybe.map .s assignment.value)
                        ]
                    )
                ]
            in
              ( swatchList, swatchList, swatchList )
        , firstRange = assignment.substring
        , ranges = []
        }
      Just suggestion ->
        { suggestion
        | ranges = assignment.substring :: suggestion.ranges
        }

updateVariableSuggestion :
  Assignment -> Maybe Suggestion -> Maybe Suggestion
updateVariableSuggestion assignment maybeSuggestion =
  Just <|
    case maybeSuggestion of
      Nothing ->
        { replacement = assignment.cleanVariable
        , swatchLists =
            let
              swatchList =
                [ Swatch "#0000ff" "#ffffff" assignment.cleanVariable ]
            in
              ( swatchList, swatchList, swatchList )
        , firstRange = assignment.variable
        , ranges = []
        }
      Just suggestion ->
        { suggestion
        | ranges = assignment.variable :: suggestion.ranges
        }

type alias ParseResult =
  { words : List Substring
  , assignments : List Assignment
  , comments : List Substring
  , indents : List Substring
  }

parse : Substring -> ParseResult
parse whole =
  let
    lineResults =
      List.map
        parseLine
        (Substring.find All (Regex.regex ".*\n?") whole)
  in let
    ( assignmentArea, chordArea ) =
      case
        splitAfterLastTrue
          (isValidAssignment << .assignment)
          lineResults
      of
        Nothing -> ( [], lineResults )
        Just x -> x
  in
    { words = List.concatMap .words chordArea
    , assignments = List.filterMap .assignment lineResults
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    }

isValidAssignment : Maybe Assignment -> Bool
isValidAssignment maybeAssignment =
  case maybeAssignment of
    Nothing -> False
    Just assignment ->
      assignment.variable.s == assignment.cleanVariable &&
        assignment.value /= Nothing &&
          not (missingSpace assignment)

missingSpace : Assignment -> Bool
missingSpace assignment =
  case assignment.value of
    Nothing -> False
    Just value ->
      String.length assignment.substring.s <=
        String.length assignment.variable.s + String.length value.s

splitAfterLastTrue : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitAfterLastTrue pred xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      case splitAfterLastTrue pred rest of
        Just ( before, after ) -> Just ( x :: before, after )
        Nothing ->
          if pred x then Just ( [ x ], rest )
          else Nothing

type alias LineResult =
  { words : List Substring
  , assignment : Maybe Assignment
  , comment : Maybe Substring
  , indent : Maybe Substring
  }

type alias Assignment =
  { variable : Substring
  , cleanVariable : String
  , value : Maybe Substring
  , substring : Substring
  }

parseLine : Substring -> LineResult
parseLine line =
  case Substring.find (AtMost 1) (Regex.regex "^#.*") line of
    comment :: _ ->
      { words = []
      , assignment = Nothing
      , comment = Just comment
      , indent = Nothing
      }
    [] ->
      let
        comment =
          case Substring.find (AtMost 1) (Regex.regex " #.*") line of
            spaceAndComment :: _ ->
              Just (Substring.dropLeft 1 spaceAndComment)
            [] ->
              Nothing
      in let
        code =
          case comment of
            Just c -> Substring.before c.i line
            Nothing -> line
      in
        case
          Substring.find (AtMost 1) (Regex.regex "^ +[^ \n]") code
        of
          indentAndChar :: _ ->
            { words = []
            , assignment = Nothing
            , comment = comment
            , indent = Just (Substring.dropRight 1 indentAndChar)
            }
          [] ->
            let assignment = findAssignment code in
              { words =
                  if isValidAssignment assignment then
                    []
                  else
                    let
                      normalWords =
                        Substring.find All (Regex.regex "[^ \n]+") code
                    in
                      if normalWords == [] then
                        []
                      else
                        normalWords ++
                          Substring.find (AtMost 1) (Regex.regex "\n$") line
              , assignment = assignment
              , comment = comment
              , indent = Nothing
              }

findAssignment : Substring -> Maybe Assignment
findAssignment code =
  case
    Substring.find (AtMost 1) (Regex.regex "^[a-zA-Z]+ *: *\n?") code
  of
    [] ->
      Nothing
    variableAndSpace :: _ ->
      let
        variable =
          { variableAndSpace
          | s = String.trimRight variableAndSpace.s
          }
      in let
        cleanVariable =
          ( String.toLower
              (String.trimRight (String.dropRight 1 variable.s))
          ) ++ ":"
      in
        if Set.member cleanVariable variables then
          let
            assignment =
              Maybe.withDefault
                variable
                ( List.head
                    (Substring.find (AtMost 1) (Regex.regex "^.*[^ \n]") code)
                )
          in
            Just
              { variable = variable
              , cleanVariable = cleanVariable
              , value =
                  let
                    afterSpace =
                      Substring.dropLeft
                        (String.length variableAndSpace.s)
                        assignment
                  in
                    if afterSpace.s == "" then Nothing else Just afterSpace
              , substring = assignment
              }
        else
          Nothing

variables : Set String
variables = Set.fromList [ "key:" ]
