module Bpm exposing (flag)

import Flag exposing (Flag)

flag : Flag Float
flag =
  { key = "bpm"
  , fromCode = fromCode
  , code = toString
  , default = 85
  }

fromCode : String -> Maybe Float
fromCode code =
  case String.toFloat code of
    Err _ ->
      Nothing
    Ok bpm ->
      Just (max 20 bpm)
