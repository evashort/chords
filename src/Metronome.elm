module Metronome exposing
  ( Metronome, getNextBeat, getTickTime, getStop, interval )
leniency : Float
leniency = 0.2

interval : Float
interval = 0.25 * 60 / 85

ticksPerBeat : Int
ticksPerBeat = 4

type alias Metronome =
  { start : Float
  , ticks : Int
  }

getNextTick : Metronome -> Float -> Int
getNextTick m t =
  min m.ticks <| ceiling <| (t - m.start) / interval - leniency

getNextBeat : Metronome -> Float -> Int
getNextBeat m t =
  ticksPerBeat * ((getNextTick m t + ticksPerBeat - 1) // ticksPerBeat)

getTickTime : Metronome -> Int -> Float
getTickTime m i =
  m.start + interval * toFloat i

getStop : Metronome -> Float
getStop m =
  1.0000000000000002 * (m.start + interval * (toFloat m.ticks + leniency))
