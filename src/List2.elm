module List2 exposing (List2, fromList1, tail1, tail2)

import List1 exposing (List1)

type alias List2 a =
  { first : a
  , second : a
  , rest : List a
  }

fromList1 : List1 a -> Maybe (List2 a)
fromList1 list1 =
  case list1.rest of
    [] ->
      Nothing
    second :: rest ->
      Just (List2 list1.first second rest)

tail1 : List2 a -> List1 a
tail1 list2 =
  List1 list2.second list2.rest

tail2 : List2 a -> Maybe (List2 a)
tail2 list2 =
  case list2.rest of
    [] ->
      Nothing
    second :: rest ->
      Just (List2 list2.second second rest)
