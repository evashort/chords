module Note exposing (Note, mapTime)

type alias Note =
  { t : Float
  , f : Float
  }

mapTime : (Float -> Float) -> Note -> Note
mapTime function { t, f } =
  { t = function t, f = f }
