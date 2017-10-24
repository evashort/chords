module Scale exposing (Scale, get, cMajor, sharpStaffRows, flatStaffRows)

type alias Scale =
  { pitches : List Int
  , period : Int
  }

get : Int -> Scale -> Int
get n scale =
  let
    degree = n % List.length scale.pitches
  in let
    octave = (n - degree) // List.length scale.pitches
  in
    scale.period * octave +
      ( case List.drop degree scale.pitches of
          pitch :: _ -> pitch
          [] -> 0
      )

cMajor : Scale
cMajor =
  { pitches = [ 0, 2, 4, 5, 7, 9, 11 ]
  , period = 12
  }

sharpStaffRows : Scale
sharpStaffRows =
  { pitches = [ 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6 ]
  , period = 7
  }

flatStaffRows : Scale
flatStaffRows =
  { pitches = [ 0, 1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6 ]
  , period = 7
  }
