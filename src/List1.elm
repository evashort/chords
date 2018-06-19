module List1 exposing (List1, fromList)

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
