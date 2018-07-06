module Cliff exposing (Cliff, nextHold, dropAfter, addHolds, addIndexedHolds)

holdLength : Float
holdLength = 0.05

type alias Hold a =
  { origin : Float
  , interval : Float
  , beat : Int
  , blob : a
  }

holdStart : Hold a -> Float
holdStart hold =
  hold.origin + hold.interval * toFloat hold.beat

type alias Cliff a = List (Hold a)

nextHold : Float -> Cliff a -> Maybe (Float, a)
nextHold time cliff =
  case cliff of
    [] ->
      Nothing
    hold :: rest ->
      let start = holdStart hold in
        if start > time then
          case nextHold time rest of
            Nothing ->
              Just ( start, hold.blob )
            x ->
              x
        else if start + holdLength >= time then
          Just ( start, hold.blob )
        else
          Nothing

dropAfter : Float -> Cliff a -> Cliff a
dropAfter time cliff =
  case cliff of
    [] ->
      cliff
    hold :: rest ->
      if holdStart hold > time then
        dropAfter time rest
      else
        cliff

addHolds : Float -> Float -> List a -> Cliff a -> Cliff a
addHolds origin interval blobs cliff =
  case cliff of
    [] ->
      addHoldsHelp origin interval 1 blobs
    hold :: rest ->
      let start = holdStart hold in
        if start > origin then
          addHolds origin interval blobs rest
        else if start == origin && hold.interval == interval then
          addHoldsHelp hold.origin interval (hold.beat + 1) blobs ++
            cliff
        else
          addHoldsHelp origin interval 1 blobs ++
            cliff

addHoldsHelp : Float -> Float -> Int -> List a -> Cliff a
addHoldsHelp origin interval firstBeat blobs =
  List.reverse
    ( List.indexedMap
        (Hold origin interval << (+) firstBeat)
        blobs
    )

addIndexedHolds : Float -> Float -> List Int -> List a -> Cliff a -> Cliff a
addIndexedHolds origin interval beats blobs cliff =
  case cliff of
    [] ->
      addIndexedHoldsHelp origin interval 0 beats blobs
    hold :: rest ->
      let start = holdStart hold in
        if start > origin then
          addIndexedHolds origin interval beats blobs rest
        else if start == origin && hold.interval == interval then
          (++)
            ( addIndexedHoldsHelp
                hold.origin
                interval
                hold.beat
                beats
                blobs
            )
            cliff
        else
          addIndexedHoldsHelp origin interval 0 beats blobs ++
            cliff

addIndexedHoldsHelp : Float -> Float -> Int -> List Int -> List a -> Cliff a
addIndexedHoldsHelp origin interval firstBeat beats blobs =
  List.reverse
    ( List.map2
        (Hold origin interval << (+) firstBeat)
        beats
        blobs
    )
