module IdChord exposing (IdChord, Msg(..))

import Chord exposing (Chord)

type alias IdChord =
  { id : Int
  , chord : Chord
  }

type Msg
  = Play IdChord
  | Stop
