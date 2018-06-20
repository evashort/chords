module MapCells exposing (mapCells)

import List1 exposing (List1)
import Replacement exposing (Replacement)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

mapCells : (String -> String) -> List Substring -> List Replacement
mapCells f lines =
  let
    rows = List.filterMap (toRow f) lines
  in
    List.map2
      Replacement
      (List.filter (not << String.isEmpty << .s) lines)
      (mapCellsHelp rows)

type alias Row = List1 Replacement

toRow : (String -> String) -> Substring -> Maybe Row
toRow f line =
  List1.fromList
    (List.map (toCell f) (Substring.find All wordRegex line))

wordRegex : Regex
wordRegex = Regex.regex "[^ ]+"

toCell : (String -> String) -> Substring -> Replacement
toCell f old =
  Replacement old (f old.s)

mapCellsHelp : List Row -> List String
mapCellsHelp rows =
  let
    groups = groupByWidth [] rows
  in let
    branches = List.concatMap mapGroupCells groups
  in let
    leaves = List.map rowLeaf rows
  in
    withDefaults branches leaves

type alias Group =
  { width : Int
  , rows : List1 Row
  }

groupByWidth : List Row -> List Row -> (List Group)
groupByWidth past rows =
  case rows of
    [] ->
      []
    row :: rest ->
      case row.rest of
        [] ->
          groupByWidth (row :: past) rest
        second :: _ ->
          let
            width = second.old.i - row.first.old.i
          in let
            currentGroup =
              takeWhile (compatibleWith width) rest
          in let
            pastGroup =
              takeWhile (compatibleWith width) past
          in let
            groupRows =
              List1.extendLeft
                (List.reverse pastGroup)
                (List1 row currentGroup)
          in
            Group width groupRows ::
              groupByWidth
                (List.reverse currentGroup)
                (List.drop (List.length currentGroup) rest)

compatibleWith : Int -> Row -> Bool
compatibleWith width row =
  case row.rest of
    [] ->
      String.length row.first.old.s < width
    second :: _ ->
      second.old.i - row.first.old.i == width

takeWhile : (a -> Bool) -> List a -> List a
takeWhile condition xs =
  case xs of
    [] ->
      []
    x :: rest ->
      if condition x then
        x :: takeWhile condition rest
      else
        []

mapGroupCells : Group -> List String
mapGroupCells group =
  let
    rest = List1.filterMap List1.tail1 group.rows
  in let
    suffixes = mapCellsHelp rest
  in let
    oldMaxLength =
      List1.maximum
        (List1.map (String.length << .s << .old << .first) group.rows)
  in let
    newMaxLength =
      List1.maximum
        (List1.map (String.length << .new << .first) group.rows)
  in let
    newWidth =
      if group.width > oldMaxLength + 1 then
        max group.width (newMaxLength + 1)
      else
        newMaxLength + 1
  in
    List.map2
      ((++) << String.padRight newWidth ' ')
      (List1.filterMap rowStem group.rows)
      suffixes

rowStem : Row -> Maybe String
rowStem row =
  if List.isEmpty row.rest then
    Nothing
  else
    Just row.first.new

rowLeaf : Row -> Maybe String
rowLeaf row =
  if List.isEmpty row.rest then
    Just row.first.new
  else
    Nothing

withDefaults : List a -> List (Maybe a) -> List a
withDefaults defaults maybes =
  case maybes of
    [] ->
      []
    Just x :: laterMaybes ->
      x :: withDefaults defaults laterMaybes
    Nothing :: laterMaybes ->
      case defaults of
        [] ->
          []
        default :: laterDefaults ->
          default ::
            withDefaults laterDefaults laterMaybes
