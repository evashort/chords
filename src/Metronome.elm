module Metronome exposing ( Metronome, nextBeat, tickTime, end, interval )

leniency : Float
leniency = 0.2

interval : Float
interval = 0.25 * 60 / 85

ticksPerBeat : Int
ticksPerBeat = 8

type alias Metronome =
  { start : Float
  , ticks : Int
  }

nextTick : Metronome -> Float -> Int
nextTick m t =
  min m.ticks <| ceiling <| (t - m.start) / interval - leniency

nextBeat : Metronome -> Float -> Int
nextBeat m t =
  let tick = nextTick m t in
    tick + ticksPerBeat - 1 - (tick - 1) % ticksPerBeat

tickTime : Metronome -> Int -> Float
tickTime m i =
  m.start + interval * toFloat i

end : Metronome -> Float
end m =
  m.start + interval * (toFloat m.ticks + leniency)
