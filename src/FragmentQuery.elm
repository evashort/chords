module FragmentQuery exposing (encode, decode)

import Regex exposing (Regex)
import Url exposing (Url)

encode : List (String, String) -> String
encode pairs =
  (++)
    "#"
    ( String.join
        "&"
        (List.map encodeHelp pairs)
    )

encodeHelp : (String, String) -> String
encodeHelp ( key, value ) =
  key ++ "=" ++ percentEncodeWithPlusSigns value

percentEncodeWithPlusSigns : String -> String
percentEncodeWithPlusSigns s =
  Regex.replace
    encodedSpace
    (always "+")
    (Url.percentEncode s)

encodedSpace : Regex
encodedSpace =
  Maybe.withDefault Regex.never (Regex.fromString "%20")

decode : String -> Url -> Maybe String
decode key url =
  case url.fragment of
    Nothing ->
      Nothing
    Just fragment ->
      decodeHelp
        (key ++ "=")
        (List.reverse (String.split "&" fragment))

decodeHelp : String -> List String -> Maybe String
decodeHelp prefix assignments =
  case assignments of
    assignment :: rest ->
      if String.startsWith prefix assignment then
        percentDecodeWithPlusSigns
          (String.dropLeft (String.length prefix) assignment)
      else
        decodeHelp prefix rest
    [] ->
      Nothing

percentDecodeWithPlusSigns : String -> Maybe String
percentDecodeWithPlusSigns s =
  Url.percentDecode
    (Regex.replace plusSign (always "%20") s)

plusSign : Regex
plusSign =
  Maybe.withDefault Regex.never (Regex.fromString "\\+")
