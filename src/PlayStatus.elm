module PlayStatus exposing
  (PlayStatus, PlaySegment, dropBefore, canDropBefore, current)

type alias PlayStatus =
  { active : Int
  , next : Int
  }

empty : PlayStatus
empty =
  { active = -1
  , next = -1
  }

type alias PlaySegment =
  { status : PlayStatus
  , stop : Float
  }

dropBefore : Float -> List PlaySegment -> List PlaySegment
dropBefore t schedule =
  case schedule of
    [] -> []
    segment :: rest ->
      if t >= segment.stop then dropBefore t rest
      else schedule

canDropBefore : Float -> List PlaySegment -> Bool
canDropBefore t schedule =
  case schedule of
    [] -> False
    segment :: _ -> t >= segment.stop

current : List PlaySegment -> PlayStatus
current schedule =
  case schedule of
    [] -> empty
    segment :: _ -> segment.status
