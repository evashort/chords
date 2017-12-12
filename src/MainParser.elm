module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import ChordParser exposing (IdChord)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import SuggestionMerge
import Swatch exposing (Swatch)

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
      (assignment.value.s /= "" && not (hasSpace assignment))
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
        , SuggestionMerge.mergeSuggestions
            assignmentReplacement
            assignmentSuggestion
            assignmentRange
            model.assignments
        ]
    )

assignmentReplacement : Assignment -> Maybe String
assignmentReplacement assignment =
  if assignment.value.s /= "" && not (hasSpace assignment) then
    Just (assignment.cleanVariable ++ " " ++ assignment.value.s)
  else if assignment.variable.s /= assignment.cleanVariable then
    Just assignment.cleanVariable
  else
    Nothing

assignmentSuggestion : Assignment -> Suggestion
assignmentSuggestion assignment =
  if assignment.value.s /= "" && not (hasSpace assignment) then
    { replacement = ""
    , swatchLists =
        let
          swatchList =
            [ Swatch "#0000ff" "#ffffff" assignment.cleanVariable
            , Swatch "#000000" "#ffffff" (" " ++ assignment.value.s)
            ]
        in
          ( swatchList, swatchList, swatchList )
    , firstRange = assignment.substring
    , ranges = []
    }
  else
    { replacement = ""
    , swatchLists =
        let
          swatchList =
            [ Swatch "#0000ff" "#ffffff" assignment.cleanVariable ]
        in
          ( swatchList, swatchList, swatchList )
    , firstRange = assignment.variable
    , ranges = []
    }

assignmentRange : Assignment -> Substring
assignmentRange assignment =
  if assignment.value.s /= "" && not (hasSpace assignment) then
    assignment.substring
  else
    assignment.variable

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
        assignment.value.s /= "" &&
          hasSpace assignment

hasSpace : Assignment -> Bool
hasSpace assignment =
  String.length assignment.substring.s >
    String.length assignment.variable.s + String.length assignment.value.s

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
  , value : Substring
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
                  Substring.dropLeft
                    (String.length variableAndSpace.s)
                    assignment
              , substring = assignment
              }
        else
          Nothing

variables : Set String
variables = Set.fromList [ "key:" ]
