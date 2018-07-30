module Chroma exposing (search)

import Chord exposing (Chord)
import IdChord exposing (IdChord)

import Dict exposing (Dict)

search : Chord -> List IdChord
search chord =
  case Dict.get (fromFlavor chord.flavor) schemes of
    Nothing ->
      []
    Just scheme ->
      List.map
        (addId << Chord.transpose chord.root)
        (List.filter (differentFlavor chord) scheme)

differentFlavor : Chord -> Chord -> Bool
differentFlavor x y =
  x.flavor /= y.flavor

addId : Chord -> IdChord
addId chord =
  case IdChord.fromChord chord of
    Nothing ->
      Debug.crash
        ("Chroma.addId: Unknown chord " ++ toString chord)
    Just idChord ->
      idChord

fromFlavor : List Int -> List Int
fromFlavor flavor =
  removeDuplicates
    (List.sort (List.map mod12 (0 :: flavor)))

removeDuplicates : List a -> List a
removeDuplicates xs =
  case xs of
    x :: y :: rest ->
      if x == y then
        removeDuplicates (y :: rest)
      else
        x :: removeDuplicates (y :: rest)
    other ->
      other

schemes : Dict (List Int) (List Chord)
schemes =
  List.foldl
    addSchemes
    Dict.empty
    (Dict.values Chord.flavors)

addSchemes :
  List Int -> Dict (List Int) (List Chord) ->
    Dict (List Int) (List Chord)
addSchemes flavor schemes =
  let chroma = fromFlavor flavor in
    List.foldl (addScheme flavor chroma) schemes chroma

addScheme :
  List Int -> List Int -> Int -> Dict (List Int) (List Chord) ->
    Dict (List Int) (List Chord)
addScheme flavor chroma offset schemes =
  Dict.update
    ( List.sort
        (List.map (mod12 << (+) -offset) chroma)
    )
    (addToScheme (Chord flavor -offset))
    schemes

mod12 : Int -> Int
mod12 x =
  x % 12

addToScheme : Chord -> Maybe (List Chord) -> Maybe (List Chord)
addToScheme chord scheme =
  Just (chord :: Maybe.withDefault [] scheme)
