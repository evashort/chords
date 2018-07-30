module Pane exposing (Pane(..), setHarpExistence)

import Ports

type Pane
  = ChordsInKey
  | Circle
  | Keyboard
  | History

setHarpExistence : Pane -> Cmd msg
setHarpExistence pane =
  Ports.setHarpExistence (pane /= History)
