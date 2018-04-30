module Train exposing
  (Train, fromCars, cars, flatten, length, filterMap, indexedMap)

type alias Train a = List (Maybe a)

fromCars : List (List a) -> Train a
fromCars cs =
  List.concat
    ( List.intersperse
        [ Nothing ]
        (List.map (List.map Just) cs)
    )

cars : Train a -> List (List a)
cars train =
  let ( c, cs ) = carsHelp train in
    c :: cs

carsHelp : Train a -> (List a, List (List a))
carsHelp train =
  case train of
    [] ->
      ( [], [] )
    Nothing :: rest ->
      let ( c, cs ) = carsHelp rest in
        ( [], c :: cs )
    Just x :: rest ->
      let ( c, cs ) = carsHelp rest in
        ( x :: c, cs )

flatten : Train a -> List a
flatten train =
  List.filterMap identity train

length : Train a -> Int
length train =
  List.length (flatten train)

filterMap : (a -> Maybe b) -> Train a -> Train b
filterMap f train =
  List.filterMap (flipMaybes << Maybe.map f) train

flipMaybes : Maybe (Maybe a) -> Maybe (Maybe a)
flipMaybes mmx =
  if mmx == Nothing then
    Just Nothing
  else if mmx == Just Nothing then
    Nothing
  else
    mmx

indexedMap : (Int -> a -> b) -> Train a -> Train b
indexedMap f train =
  List.map2
    (Maybe.map << f)
    (List.scanl ((+) << maybeLength) 0 train)
    train

maybeLength : Maybe a -> Int
maybeLength mx =
  if mx == Nothing then 0 else 1
