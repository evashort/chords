module TickTime exposing ( nextBeat, get, toTick )

leniency : Float
leniency = 0.2

interval : Float
interval = 0.25 * 60 / 85

ticksPerBeat : Int
ticksPerBeat = 8

next : Float -> Float -> Int
next start t =
  ceiling ((t - start) / interval - leniency)

nextBeat : Float -> Float -> Int
nextBeat start t =
  let tick = next start t in
    tick + ticksPerBeat - 1 - (tick - 1) % ticksPerBeat

get : Float -> Int -> Float
get start i =
  start + interval * toFloat i

toTick : Float -> Float -> Int
toTick start t =
  floor ((t - start) / interval)
