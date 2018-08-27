module Bpm exposing (flag)

import Flag exposing (Flag)

flag : Flag (Maybe Float)
flag =
  { key = "bpm"
  , fromCode = fromCode
  , toCode = toCode
  , default = Nothing
  }

fromCode : String -> Maybe (Maybe Float)
fromCode code =
  if String.toLower code == "default" then
    Just Nothing
  else
    case String.toFloat code of
      Nothing ->
        Nothing
      Just bpm ->
        Just (Just (max 20 bpm))

toCode : Maybe Float -> String
toCode maybeBpm =
  case maybeBpm of
    Nothing ->
      "default"
    Just bpm ->
      String.fromFloat bpm
