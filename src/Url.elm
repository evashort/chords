module Url exposing ( percentEncode, percentDecode, hashParamValue )

import Native.Url

import Navigation exposing (Location)
import Regex exposing (Regex, HowMany(..))

percentEncode : String -> String
percentEncode s =
  Regex.replace
    All
    encodedSpace
    (always "+")
    (Native.Url.percentEncode s)

encodedSpace : Regex
encodedSpace = Regex.regex "%20"

percentDecode : String -> Maybe String
percentDecode s =
  Native.Url.percentDecode
    (Regex.replace All plusSign (always "%20") s)

plusSign : Regex
plusSign = Regex.regex "\\+"

hashParamValue : String -> Location -> Maybe String
hashParamValue key location =
  hashParamValueHelp
    (key ++ "=")
    (List.reverse (String.split "&" (String.dropLeft 1 location.hash)))

hashParamValueHelp : String -> List String -> Maybe String
hashParamValueHelp prefix assignments =
  case assignments of
    assignment :: rest ->
      if String.startsWith prefix assignment then
        percentDecode (String.dropLeft (String.length prefix) assignment)
      else
        hashParamValueHelp prefix rest
    [] ->
      Nothing
