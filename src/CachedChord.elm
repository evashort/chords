module CachedChord exposing
  ( CachedChord, fg, bg, swatch, borderOpacity, shineOpacity, codeName
  , view, fromChord, transposeRootOctave
  )

import Chord exposing (Chord)
import Flavor exposing (Flavor)
import Note exposing (Note)
import StaffMap exposing (StaffMap)
import Swatch exposing (Swatch)

import Html exposing (Html, text, sup, sub)

type alias CachedChord =
  { chord : Chord
  , i : Int
  , flavor : Flavor
  , staffMap : StaffMap
  , namesake : Note
  , root : Note
  }

transposeRootOctave : Int -> Int -> CachedChord -> CachedChord
transposeRootOctave lowestNote lnOffset cache =
  let
    root = Chord.get cache.chord 0
  in let
    x = (root - lowestNote) % 12 - lnOffset
  in
    transposeByOctaves -((x - x % 12) // 12) cache

transposeByOctaves : Int -> CachedChord -> CachedChord
transposeByOctaves octaves cache =
  if octaves == 0 then
    cache
  else
    { cache
    | chord = List.map ((+) (12 * octaves)) cache.chord
    , staffMap = List.map ((+) (7 * octaves)) cache.staffMap
    }

fg : CachedChord -> String
fg x =
  x.flavor.fg

bg : Int -> CachedChord -> String
bg key x =
  let ( a, b, c ) = x.flavor.bg in
    case (Chord.get x.chord x.i - key) % 3 of
      0 -> a
      1 -> b
      _ -> c

swatch : Int -> Int -> CachedChord -> Swatch
swatch key lowestNote cache =
  { fg = fg cache, bg = bg key cache, s = codeName lowestNote cache }

borderOpacity : CachedChord -> String
borderOpacity x =
  if x.flavor.fg == "#ffffff" then "0.8" else "0.3"

shineOpacity : CachedChord -> String
shineOpacity x =
  if x.flavor.fg == "#ffffff" then "0.6" else "0.7"

codeName : Int -> CachedChord -> String
codeName lowestNote x =
  case x.root.codeName ++ octaveName lowestNote x of
    "" ->
      x.namesake.codeName ++ x.flavor.codeName
    codeRoot ->
      x.namesake.codeName ++ x.flavor.codeName ++ "/" ++ codeRoot

view : Int -> CachedChord -> List (Html msg)
view lowestNote x =
  case
    ( x.flavor.superscript
    , x.root.prettyName ++ octaveName lowestNote x
    )
  of
    ( "", "" ) ->
      [ text (x.namesake.prettyName ++ x.flavor.prettyName) ]
    ( _, "" ) ->
      [ text (x.namesake.prettyName ++ x.flavor.prettyName)
      , sup [] [ text x.flavor.superscript ]
      ]
    ( "", prettyRoot ) ->
      [ text (x.namesake.prettyName ++ x.flavor.prettyName ++ "/")
      , sub [] [ text prettyRoot ]
      ]
    ( _, prettyRoot ) ->
      [ text (x.namesake.prettyName ++ x.flavor.prettyName)
      , sup [] [ text x.flavor.superscript ]
      , text "⁄"
      , sub [] [ text prettyRoot ]
      ]

octaveName : Int -> CachedChord -> String
octaveName lowestNote x =
  let root = Chord.get x.chord 0 in
    if lowestNote <= root && root < lowestNote + 12 then
      ""
    else
      toString ((root - root % 12) // 12 - 2)

fromChord : Chord -> CachedChord
fromChord chord =
  let
    ( i, maybeFlavor ) = Flavor.get (Chord.intervals chord)
  in let
    flavor = Maybe.withDefault Flavor.errorFlavor maybeFlavor
  in let
    staffNamesake = StaffMap.get flavor.staffMap (Chord.get chord i)
  in let
    staffMap =
      StaffMap.invert
        -i
        (List.map ((+) staffNamesake) flavor.staffOffsets)
  in
    { chord = chord
    , i = i
    , flavor = flavor
    , staffMap = staffMap
    , namesake =
        Note.init (Chord.get chord i) (StaffMap.get staffMap i)
    , root =
        if i == 0 then
          { codeName = "", prettyName = "" }
        else
          Note.init (Chord.get chord 0) (StaffMap.get staffMap 0)
    }
