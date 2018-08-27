module Zipper exposing (doubleZip, DoubleZipped)

type alias DoubleZipped a b c =
  { left : List c
  , upper : List a
  , lower : List b
  , right : List c
  }

doubleZip : (a -> b -> Maybe c) -> List a -> List b -> DoubleZipped a b c
doubleZip zipper upper lower =
  let
    leftZipped = zip zipper upper lower
  in
  let
    rightZipped =
      zip
        zipper
        (List.reverse leftZipped.upper)
        (List.reverse leftZipped.lower)
  in
    { left = leftZipped.left
    , upper = List.reverse rightZipped.upper
    , lower = List.reverse rightZipped.lower
    , right = List.reverse rightZipped.left
    }

type alias Zipped a b c =
  { left : List c
  , upper : List a
  , lower : List b
  }

zip : (a -> b -> Maybe c) -> List a -> List b -> Zipped a b c
zip zipper upper lower =
  case ( upper, lower ) of
    ( x :: upperRest, y :: lowerRest ) ->
      case zipper x y of
        Just z ->
          let zippedRest = zip zipper upperRest lowerRest in
            { zippedRest | left = z :: zippedRest.left }
        Nothing ->
          { left = [], upper = upper, lower = lower }
    _ ->
      { left = [], upper = upper, lower = lower }
