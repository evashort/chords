module Schedule exposing (Schedule, Segment, get, next, add, dropBefore)

type alias Schedule a =
  { stop : Int
  , segments : List (Segment a)
  }

type alias Segment a =
  { x : a
  , start : Int
  }

get : Int -> Schedule a -> Maybe (Segment a)
get tick schedule =
  if tick >= schedule.stop then Nothing
  else getHelp tick schedule.segments

getHelp : Int -> List (Segment a) -> Maybe (Segment a)
getHelp tick segments =
  case segments of
    [] -> Nothing
    segment :: rest ->
      if tick >= segment.start then Just segment
      else getHelp tick rest

next : Int -> Schedule a -> Maybe (Segment a)
next tick schedule =
  if tick >= schedule.stop then Nothing
  else nextHelp Nothing tick schedule.segments

nextHelp : Maybe (Segment a) -> Int -> List (Segment a) -> Maybe (Segment a)
nextHelp currentNext tick segments =
  case segments of
    [] -> Nothing
    segment :: rest ->
      if tick >= segment.start then currentNext
      else nextHelp (Just segment) tick rest

add : Int -> Segment a -> Schedule a -> Schedule a
add stop segment schedule =
  { schedule
  | stop = stop
  , segments = segment :: takeBefore segment.start schedule.segments
  }

takeBefore : Int -> List (Segment a) -> List (Segment a)
takeBefore tick segments =
  case segments of
    [] -> []
    { x, start } :: rest ->
      if tick > start then segments
      else takeBefore tick rest

dropBefore : Int -> Schedule a -> Schedule a
dropBefore tick schedule =
  { schedule
  | segments =
      if tick >= schedule.stop then []
      else dropBeforeHelp tick schedule.segments
  }

dropBeforeHelp : Int -> List (Segment a) -> List (Segment a)
dropBeforeHelp tick segments =
  case segments of
    [] -> []
    segment :: rest ->
      if tick >= segment.start then [ { segment | start = tick } ]
      else segment :: dropBeforeHelp tick rest
