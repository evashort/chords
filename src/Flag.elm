module Flag exposing (Flag, parse, insert, remove)

import Replacement exposing (Replacement)
import Submatches exposing (submatches)
import Substring exposing (Substring)

import Regex exposing (Regex)
import Set exposing (Set)

type alias Flag a =
  { key : String
  , fromCode : String -> Maybe a
  , default : a
  , code : a -> String
  }

parse : Flag a -> List Substring -> a
parse flag lines =
  Maybe.withDefault
    flag.default
    ( oneOfMap
        (Maybe.andThen (parseValue flag) << getValueString flag.key)
        (List.reverse lines)
    )

getValueString : String -> Substring -> Maybe String
getValueString key =
  -- implicit line argument causes regex to be cached for multiple lines
  let
    regex =
      Regex.regex ("^" ++ Regex.escape key ++ ": ([^ ].*)")
  in
    Maybe.withDefault Nothing <<
      List.head << submatches regex << .s

parseValue : Flag a -> String -> Maybe a
parseValue flag valueString =
  case flag.fromCode valueString of
    Nothing ->
      Nothing
    Just value ->
      if flag.code value == valueString then
        Just value
      else
        Nothing

oneOfMap : (a -> Maybe b) -> List a -> Maybe b
oneOfMap f xs =
  case xs of
    [] -> Nothing
    x :: rest ->
      case f x of
        Nothing -> oneOfMap f rest
        y -> y

insert : Flag a -> a -> List Substring -> Maybe Replacement
insert flag newValue lines =
  let reverseLines = List.reverse lines in
    ( if newValue == flag.default then
        insertDefaultHelp
      else
        insertHelp
    )
      { getOld = getValueString flag.key
      , new = flag.code newValue
      , flag = flag
      , allLines = reverseLines
      }
      reverseLines

type alias Args a =
  { getOld : Substring -> Maybe String
  , new : String
  , flag : Flag a
  , allLines : List Substring
  }

insertDefaultHelp : Args a -> List Substring -> Maybe Replacement
insertDefaultHelp args lines =
  case lines of
    [] ->
      Nothing
    line :: rest ->
      case args.getOld line of
        Nothing ->
          insertDefaultHelp args rest
        Just old ->
          if old == args.new then
            Nothing
          else if parseValue args.flag old == Nothing then
            insertDefaultHelp args rest
          else
            let
              deleteResult =
                case
                  oneOfMap
                    (Maybe.andThen (parseValue args.flag) << args.getOld)
                    rest
                of
                  Nothing ->
                    deleteLine args.allLines line
                  _ ->
                    Nothing
            in
              if deleteResult == Nothing then
                Just
                  ( Replacement
                      (Substring.right (String.length old) line)
                      args.new
                  )
              else
                deleteResult

deleteLine : List Substring -> Substring -> Maybe Replacement
deleteLine lines line =
  case lines of
    terminator :: lastLine :: rest ->
      if lastLine.i == line.i then
        if terminator.i == Substring.stop lastLine + 1 then
          Just (Replacement lastLine "")
        else
          Nothing
      else
        deleteLineHelp line.i lastLine.i rest
    x ->
      Debug.crash ("Flag.deleteLine: Fewer than two lines: " ++ toString x)

deleteLineHelp : Int -> Int -> List Substring -> Maybe Replacement
deleteLineHelp target nextLineStart lines =
  case lines of
    [] ->
      Debug.crash ("Flag.deleteLine: Target not found: " ++ toString target)
    line :: rest ->
      if line.i == target then
        if nextLineStart == Substring.stop line + 1 then
          Just (Replacement { line | s = line.s ++ "\n" } "")
        else
          Nothing
      else
        deleteLineHelp target line.i rest

insertHelp : Args a -> List Substring -> Maybe Replacement
insertHelp args lines =
  case lines of
    [] ->
      Just (addLine args.flag.key args.new args.allLines)
    line :: rest ->
      case args.getOld line of
        Nothing ->
          insertHelp args rest
        Just old ->
          if old == args.new then
            Nothing
          else if parseValue args.flag old == Nothing then
            insertHelp args rest
          else
            Just
              ( Replacement
                  (Substring.right (String.length old) line)
                  args.new
              )

addLine : String -> String -> List Substring -> Replacement
addLine key value lines =
  case lines of
    [] ->
      Debug.crash "Flag.addLine: Lines list empty"
    [ firstLine ] ->
      Replacement
        (Substring firstLine.i "")
        (key ++ ": " ++ value ++ "\n")
    nextLine :: line :: rest ->
      case getKey line of
        Nothing ->
          addLine key value (line :: rest)
        Just currentKey ->
          if key >= currentKey then
            Replacement
              (Substring (nextLine.i - 1) "")
              ("\n" ++ key ++ ": " ++ value)
          else
            addLine key value (line :: rest)

getKey : Substring -> Maybe String
getKey line =
  case submatches keyRegex line.s of
    [ Just key, Nothing ] ->
      if Set.member key keys then
        Just key
      else
        Nothing
    _  ->
      Nothing

remove : List Substring -> List Substring
remove lines =
  List.reverse (takeWhile keyless (List.reverse lines))

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

keyless : Substring -> Bool
keyless line =
  case submatches keyRegex line.s of
    [ Just key, Nothing ] ->
      not (Set.member key keys)
    _  ->
      True

keyRegex : Regex
keyRegex = Regex.regex "^([^:]+):([^ ])?"

keys : Set String
keys =
  Set.fromList [ "bpm", "octave", "scale" ]
