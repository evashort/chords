module IdChord exposing (IdChord, fromChord, count)

import Chord exposing (Chord)

import Dict exposing (Dict)

type alias IdChord =
  { id : Int
  , chord : Chord
  }

fromChord : Chord -> Maybe IdChord
fromChord chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      Nothing
    Just i ->
      Just (IdChord (12 * i + chord.root) chord)

count : Int
count = 12 * Dict.size schemes

schemes : Dict (List Int) Int
schemes =
  Dict.fromList
    (List.indexedMap reversedTuple Chord.flavors)

reversedTuple : a -> b -> ( b, a )
reversedTuple x y =
  ( y, x )
