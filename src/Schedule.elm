module Schedule exposing
  (Schedule, ScheduledChord, get, isStop, next, add, dropBefore)

import Chord exposing (Chord)

type alias Schedule = List ScheduledChord

type alias ScheduledChord =
  { chord : Chord
  , stop : Int
  }

get : Int -> Schedule -> Maybe Chord
get tick schedule =
  case schedule of
    [] -> Nothing
    { chord, stop } :: rest ->
      if tick < stop then Just chord
      else get tick rest

isStop : Int -> Schedule -> Bool
isStop tick schedule =
  case schedule of
    [] -> False
    { chord, stop } :: rest ->
      if tick <= stop then tick == stop
      else isStop tick rest

next : Int -> Schedule -> Maybe Chord
next tick schedule =
  case schedule of
    [] -> Nothing
    { chord, stop } :: rest ->
      if tick < stop then Maybe.map .chord (List.head rest)
      else next tick rest

add : Int -> ScheduledChord -> Schedule -> Schedule
add start y schedule =
  case schedule of
    [] -> [ y ]
    x :: rest ->
      if start < x.stop then [ { x | stop = start }, y ]
      else x :: add start y rest

dropBefore : Int -> Schedule -> Schedule
dropBefore tick schedule =
  case schedule of
    [] -> []
    { chord, stop } :: rest ->
      if tick < stop then schedule
      else dropBefore tick rest
