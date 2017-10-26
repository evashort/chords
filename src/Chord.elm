module Chord exposing (Chord, fromRawName, arpeggio, prettyName, fg, bg)

import StaffMap exposing (StaffMap)

import Dict exposing (Dict)

type alias Chord = List Int

fromRawName : String -> Maybe Chord
fromRawName name =
  case name of
    "C" -> Just [ 48, 52, 55 ]
    "Dm" -> Just [ 50, 53, 57 ]
    "Em" -> Just [ 52, 55, 59 ]
    "F" -> Just [ 53, 57, 60 ]
    "G" -> Just [ 55, 59, 62 ]
    "Am" -> Just [ 57, 60, 64 ]
    "Bo" -> Just [ 59, 62, 65 ]
    _ -> Nothing

fg : Chord -> String
fg chord =
  case chord of
    [ 59, 62, 65 ] -> "#ffffff"
    _ -> "#000000"

-- http://www.colourlovers.com/palette/324465/Pastel_Rainbow
-- blue and orange from http://www.colourlovers.com/palette/36070/pastel_rainbow
bg : Chord -> String
bg chord =
  case chord of
    [ 48, 52, 55 ] -> "#f8facd"
    [ 50, 53, 57 ] -> "#eccdfa"
    [ 52, 55, 59 ] -> "#d2facd"
    [ 53, 57, 60 ] -> "#facdcd"
    [ 55, 59, 62 ] -> "#c9ffff"
    [ 57, 60, 64 ] -> "#ffe7c9"
    [ 59, 62, 65 ] -> "#005e93"
    _ -> "#ffffff"

arpeggio : Chord -> List Int
arpeggio chord =
  let
    indices =
      case List.length chord of
        3 -> [ 3, 2, 1, 2, 0, 2, 1, 2 ]
        4 -> [ 4, 3, 2, 1, 0, 1, 2, 3 ]
        _ -> List.range 0 (List.length chord - 1)
  in
    multiGet indices chord

prettyName : Chord -> String
prettyName chord =
  let
    namesakeIndex = getNamesakeIndex (getIntervals chord)
  in let
    namesakeChord = invert namesakeIndex chord
  in let
    staffMap = StaffMap.invert -namesakeIndex (getStaffMap namesakeChord)
  in let
    flavorName =
      Maybe.withDefault "?" <|
        Dict.get (getIntervals namesakeChord) flavorNames
  in let
    staffChord = { chord = chord, staffMap = staffMap }
  in let
    inversionName =
      if namesakeIndex == 0 then
        ""
      else
        "/" ++ getNotePrettyName 0 staffChord ++ getOctaveName (get 0 chord)
  in
    getNotePrettyName namesakeIndex staffChord ++ flavorName ++ inversionName

-- BASICS

getIntervals : Chord -> List Int
getIntervals chord =
  List.map2 (-) (List.drop 1 chord) chord

invert : Int -> Chord -> Chord
invert n chord =
  multiGet (List.range n (n + List.length chord - 1)) chord

multiGet : List Int -> Chord -> List Int
multiGet ns chord =
  List.map ((flip get) chord) ns

get : Int -> Chord -> Int
get n chord =
  let
    i = n % List.length chord
  in let
    octave = (n - i) // List.length chord
  in
    case List.drop i chord of
      pitch :: _ -> 12 * octave + pitch
      [] -> 0

-- FINDING CANONICAL INVERSION

getNamesakeIndex : List Int -> Int
getNamesakeIndex intervals =
  case intervals of
    [ 5, 5 ] -> 1
    [ 5, 2 ] -> 0
    [ 2, 5 ] -> 0
    [ 4, 4, 2 ] -> 0
    [ 3, 3, 5 ] -> 0
    [ 4, 2, 4 ] -> 0
    _ ->
      case where_ (List.map (not << isThird) intervals) of
        [ i ] -> i + 1
        [ i, j ] -> j + 1
        _ -> 0

isThird : Int -> Bool
isThird interval =
  interval == 3 || interval == 4

where_ : List Bool -> List Int
where_ = whereHelp 0

whereHelp : Int -> List Bool -> List Int
whereHelp i xs =
  case xs of
    [] -> []
    True :: rest -> i :: whereHelp (i + 1) rest
    False :: rest -> whereHelp (i + 1) rest

-- ASSIGNING NOTES TO STAFF ROWS

type alias StaffChord =
  { chord : Chord
  , staffMap : StaffMap
  }

getStaffMap : Chord -> StaffMap
getStaffMap chord =
  let
    root = get 0 chord
  in let
    staffRoots =
      List.map (StaffMap.get root) [ StaffMap.cFloor, StaffMap.cCeiling ]
  in let
    whiteChords = List.map (whitenChord chord) (unique staffRoots)
  in
    case argmin (getDistance chord << .chord) whiteChords of
      Nothing -> [ 0 ]
      Just whiteChord -> whiteChord.staffMap

whitenChord : List Int -> Int -> StaffChord
whitenChord chord staffRoot =
  let
    staffMap = StaffMap.fromPitchIntervals staffRoot (getIntervals chord)
  in
    { chord = multiGet staffMap cScale
    , staffMap = staffMap
    }

cScale : Chord
cScale = [ 0, 2, 4, 5, 7, 9, 11 ]

unique : List a -> List a
unique xs =
  case xs of
    x :: y :: rest ->
      if x == y then unique (y :: rest) else x :: unique (y :: rest)
    _ -> xs

argmin : (a -> comparable) -> List a -> Maybe a
argmin f xs =
  case List.map2 (,) (List.map f xs) xs of
    pair :: rest ->
      Just (Tuple.second (List.foldl minFirst pair rest))
    [] ->
      Nothing

minFirst : (comparable, a) -> (comparable, a) -> (comparable, a)
minFirst x y =
  if Tuple.first x < Tuple.first y then x else y

getDistance : Chord -> Chord -> Int
getDistance a b =
  List.sum (List.map abs (List.map2 (-) a b))

-- CHORD NAME COMPONENTS

getNotePrettyName : Int -> StaffChord -> String
getNotePrettyName i { chord, staffMap } =
  let staffRow = StaffMap.get i staffMap in
    getLetter staffRow ++
      getAccidentalPrettyName (get i chord - get staffRow cScale)

getLetter : Int -> String
getLetter staffRow =
  let i = staffRow % 7 in
    String.slice i (i + 1) "CDEFGAB"

getAccidentalPrettyName : Int -> String
getAccidentalPrettyName accidental =
  if accidental < 0 then
    tally -accidental "♭" "𝄫"
  else
    tally accidental "♯" "𝄪"

tally : Int -> String -> String -> String
tally n one two =
  String.repeat (n % 2) one ++ String.repeat (n // 2) two

getOctaveName : Int -> String
getOctaveName pitch =
  case (pitch - pitch % 12) // 12 - 2 of
    2 -> ""
    octave -> toString octave

flavorNames : Dict (List Int) String
flavorNames =
  Dict.fromList
    [ ( [ 4, 3 ], "" )
    , ( [ 3, 4 ], "m" )
    , ( [ 3, 3 ], "^o" )
    , ( [ 4, 4 ], "+" )
    , ( [ 5, 2 ], "^sus4" )
    , ( [ 2, 5 ], "^sus2" )
    , ( [ 4, 3, 3 ], "7" )
    , ( [ 3, 4, 3 ], "m^7" )
    , ( [ 3, 3, 4 ], "m^7♭5" )
    , ( [ 4, 3, 4 ], "M^7" )
    , ( [ 3, 3, 3 ], "^o7" )
    , ( [ 3, 4, 4 ], "m^M7" )
    , ( [ 4, 4, 3 ], "+^M7" )
    , ( [ 4, 4, 2 ], "+^7" )
    , ( [ 3, 3, 5 ], "m^M7♭5" )
    , ( [ 4, 2, 4 ], "^7♭5" )
    ]
