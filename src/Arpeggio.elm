module Arpeggio exposing (Arpeggio, get, multiGet)

import Array exposing (Array)

type alias Arpeggio =
  { start : Int
  , intro : Array (List Int)
  , loop : Array (List Int)
  }

multiGet : List Int -> Arpeggio -> List (List Int)
multiGet ns arpeggio =
  List.map ((flip get) arpeggio) ns

get : Int -> Arpeggio -> List Int
get i { start, intro, loop } =
  if i < start then
    []
  else
    case Array.get (i - start) intro of
      Just x ->
        x
      Nothing ->
        case
          Array.get
            ((i - start - Array.length intro) % Array.length loop)
            loop
        of
          Just x ->
            x
          Nothing ->
            []
