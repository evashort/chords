module PlayStatus exposing (PlayStatus, IdChord, Msg(..))

import Chord exposing (Chord)

type alias PlayStatus =
  { active : Int
  , next : Int
  , stoppable : Bool
  }

type alias IdChord =
  { id : Int
  , chord : Chord
  }

type Msg
  = Play IdChord
  | Stop
