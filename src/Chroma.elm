module Chroma exposing (search, extendedSearch, subsetSearch)

import Chord exposing (Chord)
import IdChord exposing (IdChord)
import Name
import Pitch
import SharpCount

import Dict exposing (Dict)

search : Chord -> List (String, IdChord)
search chord =
  case Dict.get (fromFlavor chord.flavor) fullSchemes of
    Nothing ->
      []
    Just schemeList ->
      List.map
        (Tuple.pair "Inversion" << addId << Chord.transpose chord.root)
        (List.filter (differentFlavor chord) schemeList)

differentFlavor : Chord -> Chord -> Bool
differentFlavor x y =
  x.flavor /= y.flavor

addId : Chord -> IdChord
addId chord =
  case IdChord.fromChord chord of
    Nothing ->
      Debug.todo
        ("Chroma.addId: Unknown chord " ++ Debug.toString chord)
    Just idChord ->
      idChord

fromFlavor : List Int -> List Int
fromFlavor flavor =
  removeDuplicates
    (List.sort (List.map (modBy 12) (0 :: flavor)))

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

fullSchemes : Dict (List Int) (List Chord)
fullSchemes =
  List.foldr addSchemes Dict.empty Chord.flavors

addSchemes :
  List Int -> Dict (List Int) (List Chord) ->
    Dict (List Int) (List Chord)
addSchemes flavor schemes =
  let
    chroma = fromFlavor flavor
  in
  let
    offsets =
      case flavor of
        [ 3, 6, 9 ] ->
          [ 0 ]
        [ 4, 8 ] ->
          [ 0 ]
        _ ->
          chroma
  in
    List.foldl (addScheme flavor chroma) schemes offsets

addScheme :
  List Int -> List Int -> Int -> Dict (List Int) (List Chord) ->
    Dict (List Int) (List Chord)
addScheme flavor chroma offset schemes =
  Dict.update
    ( List.sort
        (List.map (modBy 12 << (+) -offset) chroma)
    )
    (addToList (Chord flavor -offset))
    schemes

addToList : a -> Maybe (List a) -> Maybe (List a)
addToList item maybeList =
  Just (item :: Maybe.withDefault [] maybeList)

-- Extended search

type alias PartialScheme =
  { chord : Chord
  , missingPitch : Int
  , sharpCount : Int
  }

extendedSearch : Chord -> List (String, IdChord)
extendedSearch chord =
  case Dict.get (fromFlavor chord.flavor) partialSchemes of
    Nothing ->
      []
    Just schemeList ->
      List.map (interpretPartialScheme chord) schemeList

interpretPartialScheme : Chord -> PartialScheme -> (String, IdChord)
interpretPartialScheme chord scheme =
  ( String.concat
      [ Pitch.view
          scheme.sharpCount
          (modBy 12 (chord.root + scheme.missingPitch))
      , " added"
      ]
  , addId (Chord.transpose chord.root scheme.chord)
  )

partialSchemes : Dict (List Int) (List PartialScheme)
partialSchemes =
  List.foldr addPartialSchemes Dict.empty interestingFlavors

addPartialSchemes :
  List Int -> Dict (List Int) (List PartialScheme) ->
    Dict (List Int) (List PartialScheme)
addPartialSchemes flavor schemes =
  let
    chroma = fromFlavor flavor
    sharpCount = Name.sharpCount flavor
  in
  let
    sharpCounts =
      SharpCount.fromFlavor sharpCount flavor
  in
  let
    missingPitches =
      List.map2
        Tuple.pair
        (List.map (modBy 12) (0 :: flavor))
        sharpCounts
  in
    List.foldl
      (addPartialSchemesHelp flavor chroma)
      schemes
      missingPitches

addPartialSchemesHelp :
  List Int -> List Int -> (Int, Int) -> Dict (List Int) (List PartialScheme) ->
    Dict (List Int) (List PartialScheme)
addPartialSchemesHelp flavor chroma ( missingPitch, sharpCount ) schemes =
  let
    partialChroma =
      List.filter ((/=) missingPitch) chroma
  in
    List.foldl
      (addPartialScheme flavor missingPitch sharpCount partialChroma)
      schemes
      partialChroma

addPartialScheme :
  List Int -> Int -> Int -> List Int -> Int ->
    Dict (List Int) (List PartialScheme) ->
    Dict (List Int) (List PartialScheme)
addPartialScheme flavor missingPitch sharpCount chroma offset schemes =
  Dict.update
    ( List.sort
        (List.map (modBy 12 << (+) -offset) chroma)
    )
    ( addToList
        { chord = Chord flavor -offset
        , missingPitch = missingPitch - offset
        , sharpCount = sharpCount
        }
    )
    schemes

interestingFlavors : List (List Int)
interestingFlavors =
  Chord.list
    [ "7"
    , "M7"
    , "m7"
    , "0"
    , "o7"
    , "mM7"
    , "9"
    , "M9"
    , "m9"
    , "7b9"
    , "6/9"
    , "M7#11"
    , "13"
    , "M13"
    , "add9"
    , "madd9"
    , "addb9"
    , "add#11"
    ]

-- Subset search

subsetSearch : Int -> Chord -> List (String, IdChord)
subsetSearch tonic chord =
  let chroma = fromFlavor chord.flavor in
    List.concatMap
      (subsetSearchHelp tonic chord chroma)
      chroma

subsetSearchHelp : Int -> Chord -> List Int -> Int -> List (String, IdChord)
subsetSearchHelp tonic chord chroma missingPitch =
  case List.filter ((/=) missingPitch) chroma of
    [] ->
      []
    offset :: rest ->
      let
        partialChroma =
          0 :: (List.map ((+) -offset) rest)
      in
        case Dict.get partialChroma interestingSchemes of
          Nothing ->
            []
          Just schemeList ->
            let
              header =
                String.concat
                  [ Pitch.view
                      (SharpCount.fromTonic tonic)
                      (modBy 12 (chord.root + missingPitch))
                  , " removed"
                  ]
            in
              List.map
                ( Tuple.pair header <<
                    addId <<
                    Chord.transpose (chord.root + offset)
                )
                schemeList

interestingSchemes : Dict (List Int) (List Chord)
interestingSchemes =
  List.foldr addSchemes Dict.empty interestingFlavors
