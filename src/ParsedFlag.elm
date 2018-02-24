module ParsedFlag exposing
  (ParsedFlag, view, getSuggestion, fromCode, isOfficial, getSkin)

import Flag exposing (Flag(..))
import Highlight exposing (Highlight)
import Skin exposing (Skin)
import Substring exposing (Substring)
import Swatch exposing (Swatch)

import Regex exposing (Regex, HowMany(..))

type alias ParsedFlag =
  { flag : Maybe Flag
  , name : Substring
  , cleanName : String
  , value : Substring
  , cleanValue : String
  , code : Substring
  , nextLineStart : Int
  }

view : Skin -> ParsedFlag -> List Highlight
view skin flag =
  List.concat
    [ if flag.name.s == flag.cleanName then
      [ Highlight "#0000ff" "#ffffff" flag.name ]
    else
      []
    , if flag.value.s == flag.cleanValue then
        case flag.flag of
          Just innerFlag ->
            [ { fg =
                  if
                    innerFlag == KeyFlag skin.key ||
                      innerFlag == LowestNoteFlag skin.lowestNote
                  then
                    "#c00000"
                  else
                    "#a0a0a0"
              , bg = "#ffffff"
              , substring = flag.value
              }
            ]
          Nothing ->
            []
    else
      []
    ]

getSuggestion : ParsedFlag -> Maybe ( List Swatch, Substring )
getSuggestion flag =
  case
    ( flag.name.s == flag.cleanName, flag.value.s == flag.cleanValue )
  of
    ( True, True ) -> Nothing
    ( False, True ) ->
      Just
        ( [ Swatch "#0000ff" "#ffffff" flag.cleanName ]
        , flag.name
        )
    ( True, False ) ->
      Just
        ( [ { fg = if flag.flag == Nothing then "#000000" else "#c00000"
            , bg = "#ffffff"
            , s = flag.cleanValue
            }
          ]
        , flag.value
        )
    ( False, False ) ->
      Just
        ( if String.startsWith "#" flag.cleanValue then
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" " "
            , Swatch "#008000" "#ffffff" flag.cleanValue
            ]
          else if flag.flag == Nothing then
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" (" " ++ flag.cleanValue)
            ]
          else
            [ Swatch "#0000ff" "#ffffff" flag.cleanName
            , Swatch "#000000" "#ffffff" " "
            , Swatch "#c00000" "#ffffff" flag.cleanValue
            ]
        , flag.code
        )

fromCode : Substring -> Maybe ParsedFlag
fromCode code =
  case
    Substring.find (AtMost 1) (Regex.regex "^[a-zA-Z]+ *: *") code
  of
    [] ->
      Nothing
    nameAndSpace :: _ ->
      let
        name =
          { nameAndSpace
          | s = String.trimRight nameAndSpace.s
          }
      in let
        cleanName =
          ( String.toLower
              (String.trimRight (String.dropRight 1 name.s))
          ) ++ ":"
      in let
        parser =
          case cleanName of
            "key:" -> Just Flag.parseKey
            "octave:" -> Just Flag.parseLowestNote
            _ -> Nothing
      in
        case parser of
          Nothing ->
            Nothing
          Just parseValue ->
            let
              value =
                Substring.dropLeft (String.length nameAndSpace.s) code
            in let
              flag = parseValue value.s
            in let
              missingSpace =
                value.s /= "" &&
                  String.length nameAndSpace.s == String.length name.s
            in
              Just
                { flag = flag
                , name =
                    if missingSpace then { name | s = "" } else name
                , cleanName = cleanName
                , value =
                    if missingSpace then { value | s = "" } else value
                , cleanValue =
                    case flag of
                      Nothing -> value.s
                      Just x -> Flag.codeValue x
                , code = code
                , nextLineStart = 0
                }

isOfficial : Maybe ParsedFlag -> Bool
isOfficial maybeFlag =
  case maybeFlag of
    Nothing -> False
    Just flag -> flag.name.s == flag.cleanName

getSkin : List ParsedFlag -> Skin
getSkin flags =
  let innerFlags = List.reverse (List.filterMap .flag flags) in
    { lowestNote =
        case List.filterMap Flag.getLowestNote innerFlags of
          [] -> 48
          x :: _ -> x
    , key =
        case List.filterMap Flag.getKey innerFlags of
          [] -> 0
          x :: _ -> x
    }
