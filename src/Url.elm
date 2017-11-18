module Url exposing ( percentEncode, percentDecode, hashParamValue )

import Native.Url

import Navigation exposing (Location)

percentEncode : String -> String
percentEncode = Native.Url.percentEncode

percentDecode : String -> Maybe String
percentDecode = Native.Url.percentDecode

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
