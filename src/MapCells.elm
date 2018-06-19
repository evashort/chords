module MapCells exposing (mapCells)

import List1 exposing (List1)
import List2 exposing (List2)
import Replacement exposing (Replacement)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

mapCells : (String -> String) -> List Substring -> List Replacement
mapCells f lines =
  let
    maybeRows =
      List.map
        (List1.fromList << Substring.find All wordRegex)
        lines
  in let
    rows = List.filterMap identity maybeRows
  in let
    rows2 = List.filterMap List2.fromList1 rows
  in let
    filteredMappedRows = mapCellsHelp f rows2
  in let
    mappedRows =
      mapCellsHelp2 f rows filteredMappedRows
  in
    List.map2
      Replacement
      (filterLike maybeRows lines)
      mappedRows

wordRegex : Regex
wordRegex = Regex.regex "[^ ]+"

mapCellsHelp :
  (String -> String) -> List (List2 Substring) -> List String
mapCellsHelp f rows =
  case rows of
    [] ->
      []
    row :: _ ->
      let
        oldWidth = firstColumnWidth row
      in let
        group =
          takeWhile
            ((==) oldWidth << firstColumnWidth)
            rows
      in let
        oldStrings = List.map (.s << .first) group
      in let
        oldMaxLength =
          maximum (List.map String.length oldStrings)
      in let
        newStrings = List.map f oldStrings
      in let
        newMaxLength =
          maximum (List.map String.length newStrings)
      in let
        newWidth =
          if oldWidth > oldMaxLength + 1 then
            max oldWidth (newMaxLength + 1)
          else
            newMaxLength + 1
      in let
        paddedStrings =
          List.map
            (String.padRight newWidth ' ')
            newStrings
      in let
        afterColumn = List.filterMap List2.tail2 group
      in let
        filteredSuffixes = mapCellsHelp f afterColumn
      in let
        group1 = List.map List2.tail1 group
      in let
        suffixes =
          mapCellsHelp2 f group1 filteredSuffixes
      in let
        mappedGroup =
          List.map2 (++) paddedStrings suffixes
      in let
        afterGroup = List.drop (List.length group) rows
      in
        mappedGroup ++ mapCellsHelp f afterGroup

mapCellsHelp2 :
  (String -> String) -> List (List1 Substring) -> List String ->
    List String
mapCellsHelp2 f rows suffixes =
  case rows of
    [] ->
      []
    row :: afterRow ->
      if List.isEmpty row.rest then
        f row.first.s ::
          mapCellsHelp2 f afterRow suffixes
      else
        case suffixes of
          [] ->
            []
          suffix :: afterSuffix ->
            suffix :: mapCellsHelp2 f afterRow afterSuffix

firstColumnWidth : List2 Substring -> Int
firstColumnWidth row =
  row.second.i - row.first.i

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

maximum : List comparable -> comparable
maximum xs =
  case List.maximum xs of
    Just x ->
      x
    Nothing ->
      Debug.crash "mapCells: maximum called on empty list"

filterLike : List (Maybe a) -> List b -> List b
filterLike model xs =
  case ( model, xs ) of
    ( Nothing :: modelRest, _ :: xRest ) ->
      filterLike modelRest xRest
    ( _ :: modelRest, x :: xRest ) ->
      x :: filterLike modelRest xRest
    _ ->
      []
