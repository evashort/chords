module Bpm exposing (flag)

import Flag exposing (Flag)

flag : Flag (Maybe Float)
flag =
  { key = "bpm"
  , fromCode = fromCode
  , code = code
  , default = Nothing
  }

fromCode : String -> Maybe (Maybe Float)
fromCode code =
  if String.toLower code == "default" then
    Just Nothing
  else
    case String.toFloat code of
      Err _ ->
        Nothing
      Ok bpm ->
        Just (Just (max 20 bpm))

code : Maybe Float -> String
code maybeBpm =
  case maybeBpm of
    Nothing ->
      "default"
    Just bpm ->
      toString bpm
