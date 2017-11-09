module StaffMap exposing (StaffMap, get, invert)

type alias StaffMap = List Int

get : StaffMap -> Int -> Int
get staffMap n =
  let
    i = n % List.length staffMap
  in let
    octave = (n - i) // List.length staffMap
  in
    case List.drop i staffMap of
      staffRow :: _ -> 7 * octave + staffRow
      [] -> 0

invert : Int -> StaffMap -> StaffMap
invert n staffMap =
  List.map (get staffMap) (List.range n (n + List.length staffMap - 1))
