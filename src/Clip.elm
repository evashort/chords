module Clip exposing (Clip, empty, startAt, append, concat, repeat, trim)

import Sound exposing (Sound)

type alias Clip =
  { sounds : List Sound
  , stop : Float
  }

empty : Clip
empty = Clip [] 0

startAt : Float -> Clip
startAt = Clip []

append : Clip -> Clip -> Clip
append firstClip secondClip =
  { sounds =
      List.append
        firstClip.sounds
        ( List.map
            (Sound.mapTime ((+) firstClip.stop))
            secondClip.sounds
        )
  , stop = firstClip.stop + secondClip.stop
  }

concat : List Clip -> Clip
concat clips =
  concatHelp 0 clips

concatHelp : Float -> List Clip -> Clip
concatHelp start clips =
  case clips of
    [] ->
      Clip [] start
    firstClip :: rest ->
      let secondClip = concatHelp (start + firstClip.stop) rest in
        { sounds =
            List.append
              ( List.map
                  (Sound.mapTime ((+) start))
                  firstClip.sounds
              )
              secondClip.sounds
        , stop = secondClip.stop
        }

repeat : Int -> Clip -> Clip
repeat cycles clip =
  { sounds =
      List.concatMap
        (repeatHelp clip)
        (List.range 0 (cycles - 1))
  , stop = clip.stop * toFloat (max 0 cycles)
  }

repeatHelp : Clip -> Int -> List Sound
repeatHelp clip cycle =
  List.map
    (Sound.mapTime ((+) (clip.stop * toFloat cycle)))
    clip.sounds

trim : Float -> Float -> Clip -> List Sound
trim start stop clip =
  List.filter
    (Sound.timeInRange start stop)
    clip.sounds
