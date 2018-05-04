module Unit exposing
  (Unit, negative, sum, code, px, ch, em, percent, fr, zero)

type alias Unit =
  { px : Float
  , em : Float
  , ch : Float
  , percent : Float
  , fr : Float
  }

negative : Unit -> Unit
negative unit =
  { px = -unit.px
  , em = -unit.em
  , ch = -unit.ch
  , percent = -unit.percent
  , fr = -unit.fr
  }

sum : List Unit -> Unit
sum units =
  { px = List.sum (List.map .px units)
  , em = List.sum (List.map .em units)
  , ch = List.sum (List.map .ch units)
  , percent = List.sum (List.map .percent units)
  , fr = List.sum (List.map .fr units)
  }

code : Unit -> String
code unit =
  case sparse unit of
    [] ->
      "0"
    [ ( name, value ) ] ->
      toString value ++ name
    ( name, value ) :: rest ->
      String.concat
        [ "calc("
        , toString value
        , name
        , String.concat (List.map term rest)
        , ")"
        ]

sparse : Unit -> List (String, Float)
sparse unit =
  List.concat
    [ if unit.px == 0 then [] else [ ( "px", unit.px ) ]
    , if unit.em == 0 then [] else [ ( "em", unit.em ) ]
    , if unit.ch == 0 then [] else [ ( "ch", unit.ch ) ]
    , if unit.percent == 0 then [] else [ ( "%", unit.percent ) ]
    , if unit.fr == 0 then [] else [ ( "fr", unit.fr ) ]
    ]

term : (String, Float) -> String
term ( name, value ) =
  if value < 0 then
    " - " ++ toString (-value) ++ name
  else
    " + " ++ toString value ++ name

px : Float -> Unit
px value =
  Unit value 0 0 0 0

em : Float -> Unit
em value =
  Unit 0 value 0 0 0

ch : Float -> Unit
ch value =
  Unit 0 0 value 0 0

percent : Float -> Unit
percent value =
  Unit 0 0 0 value 0

fr : Float -> Unit
fr value =
  Unit 0 0 0 0 value

zero : Unit
zero = Unit 0 0 0 0 0
