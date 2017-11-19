port module Selection exposing
  ( LandingPad, Selection, Model, setSelection, setLandingPad
  , removeLandingPad, checkSelection, receiveModel
  )

type alias LandingPad =
  { source : String
  , selection : Selection
  }

type alias Selection =
  { start : Int
  , stop : Int
  }

type alias Model =
  { selection : Selection
  , landingPad : Maybe LandingPad
  }

port setSelection : Selection -> Cmd msg

port setLandingPad : LandingPad -> Cmd msg -- also sets selection

port removeLandingPad : String -> Cmd msg

port checkSelection : () -> Cmd msg

port receiveModel : (Model -> msg) -> Sub msg
