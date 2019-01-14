module Metro exposing (Metro, setBpm, getTime, getBeat, nextOpening)

type alias Metro = List Region

type alias Region =
  { start : Float
  , interval : Float
  , startBeat : Float
  }

setBpm : Float -> Float -> Metro -> Metro
setBpm bpm now metro =
  case metro of
    [] ->
      [ { start = now
        , interval = 60 / bpm
        , startBeat = 0
        }
      ]
    region :: rest ->
      if region.start >= now then
        setBpm bpm now rest
      else if region.interval == 60 / bpm then
        metro
      else
        (::)
          { start = now
          , interval = 60 / bpm
          , startBeat =
              region.startBeat + (now - region.start) / region.interval
          }
          metro

getTime : Metro -> Float -> Float
getTime metro beat =
  case metro of
    [] ->
      Debug.todo "Metro.getTime: empty metro"
    [ region ] ->
      region.start + region.interval * (beat - region.startBeat)
    region :: rest ->
      if region.startBeat > beat then
        getTime rest beat
      else
        region.start + region.interval * (beat - region.startBeat)

getBeat : Metro -> Float -> Float
getBeat metro now =
  case metro of
    [] ->
      Debug.todo "Metro.getBeat: empty metro"
    [ region ] ->
      region.startBeat + (now - region.start) / region.interval
    region :: rest ->
      if region.start > now then
        getBeat rest now
      else
        region.startBeat + (now - region.start) / region.interval

nextOpening : Metro -> Float -> Int
nextOpening metro now =
  let
    closestBeat =
      openingInterval *
        round (getBeat metro now / toFloat openingInterval)
    minBeat =
      openingInterval *
        ceiling (getBeat metro (now - leniency) / toFloat openingInterval)
  in
    max closestBeat minBeat

openingInterval : Int
openingInterval = 2

leniency : Float
leniency = 0.05
