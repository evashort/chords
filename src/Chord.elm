module Chord exposing (Chord, prettyName)

import Note exposing (Note)
import Scale exposing (Scale)

import Dict exposing (Dict)

type alias Chord =
  { root : Int
  , intervals : List Int
  }

invert : Int -> Chord -> Chord
invert n chord =
  let
    start = n % List.length chord.intervals
  in let
    octaves = (n - start) // List.length chord.intervals
  in
    { root =
        chord.root + 12 * octaves +
          List.sum (List.take start chord.intervals)
    , intervals =
        List.drop start chord.intervals ++
          List.take start chord.intervals
    }

getPitches : Chord -> List Int
getPitches chord =
  List.scanl (+) chord.root <|
    List.take (List.length chord.intervals - 1) chord.intervals

getStaffRows : Int -> List Int -> List Int
getStaffRows staffRoot intervals =
  getPitches
    { root = staffRoot
    , intervals = List.map ((//) 2 << (+) 1) intervals
    }

getFlavor : Chord -> Maybe ( Int, Chord, String )
getFlavor = getFlavorHelp 0

getFlavorHelp : Int -> Chord -> Maybe ( Int, Chord, String )
getFlavorHelp rootIndex chord =
  if rootIndex >= List.length chord.intervals then
    Nothing
  else
    let inverted = invert -rootIndex chord in
      case Dict.get inverted.intervals flavorNames of
        Nothing ->
          getFlavorHelp (rootIndex + 1) chord
        Just name ->
          Just ( rootIndex, inverted, name )

prettyName : Chord -> String
prettyName chord =
  case getFlavor chord of
    Nothing ->
      "error"
    Just ( rootIndex, inverted, flavorName ) ->
      let
        pitches = getPitches inverted
      in let
        getNotes staffRoot =
          List.map2
            Note.fromPitch
            (getStaffRows staffRoot inverted.intervals)
            pitches
      in let
        lowStaffRoot = Scale.get inverted.root Scale.sharpStaffRows
      in let
        highStaffRoot = Scale.get inverted.root Scale.flatStaffRows
      in let
        notes =
          if lowStaffRoot == highStaffRoot then
            getNotes lowStaffRoot
          else
            let
              sharpNotes = getNotes lowStaffRoot
            in let
              flatNotes = getNotes highStaffRoot
            in
              if Note.sumCost flatNotes < Note.sumCost sharpNotes then
                flatNotes
              else
                sharpNotes
      in let
        namesakeName =
          case notes of
            namesake :: _ -> Note.prettyName namesake
            [] -> "Q"
      in let
        rootName =
          case List.drop rootIndex notes of
            root :: _ -> Note.prettyName root
            [] -> "Q"
      in
        namesakeName ++ flavorName ++ "/" ++
          rootName ++ getOctaveName chord.root

getOctaveName : Int -> String
getOctaveName pitch =
  case (pitch - pitch % 12) // 12 - 2 of
    2 -> ""
    octave -> toString octave

intervalLists : Dict String (List Int)
intervalLists =
  Dict.fromList
    [ ( "", [ 4, 3, 5 ] )
    , ( "m", [ 3, 4, 5 ] )
    , ( "o", [ 3, 3, 6 ] )
    , ( "+", [ 4, 4, 4 ] )
    , ( "sus", [ 5, 2, 5 ] )
    , ( "sus4", [ 5, 2, 5 ] )
    , ( "sus2", [ 2, 5, 5 ] )
    , ( "7", [ 4, 3, 3, 2 ] )
    , ( "m7", [ 3, 4, 3, 2 ] )
    , ( "m7b5", [ 3, 3, 4, 2 ] )
    , ( "M7", [ 4, 3, 4, 1 ] )
    , ( "o7", [ 3, 3, 3, 3 ] )
    , ( "mM7", [ 3, 4, 4, 1 ] )
    , ( "+M7", [ 4, 4, 3, 1 ] )
    , ( "+7", [ 4, 4, 2, 2 ] )
    , ( "mM7b5", [ 3, 3, 5, 1 ] )
    , ( "7b5", [ 4, 2, 4, 2 ] )
    ]

flavorNames : Dict (List Int) String
flavorNames =
  Dict.fromList
    [ ( [ 4, 3, 5 ], "" )
    , ( [ 3, 4, 5 ], "m" )
    , ( [ 3, 3, 6 ], "^o" )
    , ( [ 4, 4, 4 ], "+" )
    , ( [ 5, 2, 5 ], "^sus4" )
    , ( [ 2, 5, 5 ], "^sus2" )
    , ( [ 4, 3, 3, 2 ], "7" )
    , ( [ 3, 4, 3, 2 ], "m^7" )
    , ( [ 3, 3, 4, 2 ], "m^7♭5" )
    , ( [ 4, 3, 4, 1 ], "M^7" )
    , ( [ 3, 3, 3, 3 ], "^o7" )
    , ( [ 3, 4, 4, 1 ], "m^M7" )
    , ( [ 4, 4, 3, 1 ], "+^M7" )
    , ( [ 4, 4, 2, 2 ], "+^7" )
    , ( [ 3, 3, 5, 1 ], "m^M7♭5" )
    , ( [ 4, 2, 4, 2 ], "^7♭5" )
    ]
