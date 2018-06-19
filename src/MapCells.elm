module MapCells exposing (mapCells)

import Replacement exposing (Replacement)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

mapCells : (String -> String) -> List Substring -> List Replacement
mapCells f lines =
  let
    rows =
      List.map (Substring.find All wordRegex) lines
  in let
    news = mapCellsHelp f rows
  in
    List.map2 Replacement lines news

wordRegex : Regex
wordRegex = Regex.regex "[^ ]+"

mapCellsHelp : (String -> String) -> List (List Substring) -> List String
mapCellsHelp f rows =
  if List.isEmpty rows then
    []
  else
    let
      ( maybeOldWidth, group ) = takeWhileWidth Nothing rows
    in let
      oldStrings = List.map .s (List.filterMap List.head group)
    in let
      newStrings = List.map f oldStrings
    in let
      newLines =
        case
          ( maybeOldWidth
          , List.maximum (List.map String.length oldStrings)
          , List.maximum (List.map String.length newStrings)
          )
        of
          ( Just oldWidth
          , Just oldMaxLength
          , Just newMaxLength
          ) ->
            let
              newWidth =
                if oldWidth > oldMaxLength + 1 then
                  max oldWidth (newMaxLength + 1)
                else
                  newMaxLength + 1
            in let
              suffixes =
                mapCellsHelp f (List.filterMap List.tail group)
            in
              List.map2 (paddedAppend newWidth) newStrings suffixes
          _ ->
            newStrings
    in let
      diluted =
        defaultWhereTrue "" (List.map List.isEmpty group) newLines
    in let
      rest = List.drop (List.length group) rows
    in
      diluted ++ mapCellsHelp f rest

takeWhileWidth :
  Maybe Int -> List (List Substring) -> (Maybe Int, List (List Substring))
takeWhileWidth target rows =
  case rows of
    [] ->
      ( target, [] )
    row :: rest ->
      let
        width =
          case row of
            word1 :: word2 :: _ ->
              Just (word2.i - word1.i)
            _ ->
              target
      in
        if target /= Nothing && width /= target then
          ( target, [] )
        else
          let
            ( finalWidth, taken ) =
              takeWhileWidth width rest
          in
            ( finalWidth, row :: taken )

paddedAppend : Int -> String -> String -> String
paddedAppend width x y =
  if String.isEmpty y then
    x
  else
    (String.padRight width ' ' x) ++ y

defaultWhereTrue : a -> List Bool -> List a -> List a
defaultWhereTrue default mask xs =
  case ( mask, xs ) of
    ( [], _ ) ->
      []
    ( False :: rest, x :: xRest ) ->
      x :: defaultWhereTrue default rest xRest
    ( _ :: rest, _ ) ->
      default :: defaultWhereTrue default rest xs
