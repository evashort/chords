module StaffMap exposing (StaffMap, get)

type alias StaffMap = List Int

get : Int -> StaffMap -> Int
get n staffMap =
  let
    i = n % List.length staffMap
  in let
    octave = (n - i) // List.length staffMap
  in
    case List.drop i staffMap of
      staffRow :: _ -> 7 * octave + staffRow
      [] -> 0
