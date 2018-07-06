module Note exposing (Note, mapTime)

type alias Note =
  { v : Float
  , t : Float
  , f : Float
  }

mapTime : (Float -> Float) -> Note -> Note
mapTime function note =
  { note | t = function note.t }
