module Cliff exposing (Hold, Region, Cliff, nextHold, addRegion)

holdLength : Float
holdLength = 0.05

type alias Hold a =
  { beat : Int
  , blob : a
  }

type alias Region a =
  { origin : Float
  , interval : Float
  , holds : List (Hold a)
  }

holdStart : Region a -> Hold a -> Float
holdStart region hold =
  region.origin + region.interval * toFloat hold.beat

type alias Cliff a = List (Region a)

-- Going back in time, look for the first hold that's not in the future.
-- If such a hold exists and is not more than holdLength in the past,
-- return it. Otherwise return the earliest hold in the future, if any.
nextHold : Float -> Cliff a -> Maybe (Float, a)
nextHold time cliff =
  case cliff of
    [] ->
      Nothing
    region :: past ->
      if region.origin > time then -- region fully in future
        case nextHold time past of
          Nothing -> -- no earlier holds are acceptable
            case List.reverse region.holds of
              [] ->
                Nothing
              hold :: _ -> -- return earliest hold in region
                Just ( holdStart region hold, hold.blob )
          x ->
            x
      else if List.isEmpty region.holds then
        nextHold time past
      else -- region contains latest non-future hold
        nextHoldInRegion time region

nextHoldInRegion : Float -> Region a -> Maybe (Float, a)
nextHoldInRegion time region =
  case region.holds of
    [] ->
      Nothing
    hold :: past ->
      let start = holdStart region hold in
        if start > time then -- hold in future
          case
            nextHoldInRegion
              time
              { region | holds = past }
          of
            Nothing -> -- no earlier holds are acceptable
              Just ( start, hold.blob )
            x ->
              x
        else if start + holdLength >= time then
          Just ( start, hold.blob )
        else
          Nothing

addRegion : Region a -> Cliff a -> Cliff a
addRegion region cliff =
  case cliff of
    [] ->
      [ { region | holds = List.reverse region.holds } ]
    currentRegion :: past ->
      if currentRegion.origin > region.origin then
        addRegion region past -- overwrite region that has same origin
      else
        addRegionHelp
          { region | holds = List.reverse region.holds }
          currentRegion
          past

addRegionHelp : Region a -> Region a -> Cliff a -> Cliff a
addRegionHelp region currentRegion pastRegions =
  case currentRegion.holds of
    [] ->
      region :: currentRegion :: pastRegions
    hold :: past ->
      let start = holdStart currentRegion hold in
        if start > region.origin then -- hold in future
          addRegionHelp
            region
            { currentRegion | holds = past }
            pastRegions
        else if
          start == region.origin &&
            currentRegion.interval == region.interval
        then
          { currentRegion
          | holds =
              List.map (addToBeat hold.beat) region.holds ++
                currentRegion.holds
          } ::
            pastRegions
        else -- hold in past or at origin but with different interval
          region :: currentRegion :: pastRegions

addToBeat : Int -> Hold a -> Hold a
addToBeat offset { beat, blob } =
  Hold (beat + offset) blob
