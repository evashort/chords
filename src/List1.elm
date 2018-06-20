module List1 exposing
  (List1, fromList, tail1, map, maximum, extendLeft, filterMap)

type alias List1 a =
  { first : a
  , rest : List a
  }

fromList : List a -> Maybe (List1 a)
fromList list =
  case list of
    [] ->
      Nothing
    first :: rest ->
      Just (List1 first rest)

tail1 : List1 a -> Maybe (List1 a)
tail1 list1 =
  case list1.rest of
    [] ->
      Nothing
    first :: rest ->
      Just (List1 first rest)

map : (a -> b) -> List1 a -> List1 b
map f list1 =
  List1 (f list1.first) (List.map f list1.rest)

maximum : List1 comparable -> comparable
maximum list =
  case List.maximum list.rest of
    Nothing ->
      list.first
    Just x ->
      max list.first x

extendLeft : List a -> List1 a -> List1 a
extendLeft list list1 =
  case list of
    [] ->
      list1
    first :: rest ->
      List1 first (rest ++ toList list1)

filterMap : (a -> Maybe b) -> List1 a -> List b
filterMap f list1 =
  List.filterMap f (toList list1)

toList : List1 a -> List a
toList list1 =
  list1.first :: list1.rest
