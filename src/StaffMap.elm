module StaffMap exposing
  (StaffMap, get, invert, fromPitchIntervals, cFloor, cCeiling)

type alias StaffMap = List Int

invert : Int -> StaffMap -> StaffMap
invert n staffMap =
  multiGet (List.range n (n + List.length staffMap)) staffMap

multiGet : List Int -> StaffMap -> List Int
multiGet ns staffMap =
  List.map ((flip get) staffMap) ns

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

fromPitchIntervals : Int -> List Int -> StaffMap
fromPitchIntervals staffRoot pitchIntervals =
  fromStaffIntervals
    staffRoot
    (List.map ((//) 2 << (+) 1) pitchIntervals)

fromStaffIntervals : Int -> List Int -> StaffMap
fromStaffIntervals = List.scanl (+)

cFloor : StaffMap
cFloor = [ 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6 ]

cCeiling : StaffMap
cCeiling = [ 0, 1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6 ]
