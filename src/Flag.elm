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
  let
    reverseLines = List.reverse lines
  in let
    lineInfos =
      List.filterMap
        identity
        ( List.map2
            (getLineInfo flag (getValueString flag.key))
            (List.drop 1 reverseLines)
            reverseLines
        )
  in
    case lineInfos of
      [] ->
        if newValue == flag.default then
          Nothing
        else
          Just
            (addLine flag.key (flag.code newValue) reverseLines)
      { line, hasComment, valueString, value } :: rest ->
        let
          previousValue =
            case rest of
              [] ->
                flag.default
              previous :: _ ->
                previous.value
        in
          if value == newValue then
            Nothing
          else if not hasComment && newValue == previousValue then
            Just
              { old = { line | s = line.s ++ "\n" }
              , new = ""
              }
          else
            Just
              { old =
                  Substring.right (String.length valueString) line
              , new = flag.code newValue
              }

type alias LineInfo a =
  { line : Substring
  , hasComment : Bool
  , valueString : String
  , value : a
  }

getLineInfo :
  Flag a -> (Substring -> Maybe String) -> Substring -> Substring ->
    Maybe (LineInfo a)
getLineInfo flag valueStringGetter line nextLine =
  case valueStringGetter line of
    Nothing ->
      Nothing
    Just valueString ->
      case parseValue flag valueString of
        Nothing ->
          Nothing
        Just value ->
          Just
            { line = line
            , hasComment =
                nextLine.i > Substring.stop line + 1
            , valueString = valueString
            , value = value
            }

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
              (Substring nextLine.i "")
              (key ++ ": " ++ value ++ "\n")
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
  Set.fromList [ "bpm", "key", "lowest" ]
