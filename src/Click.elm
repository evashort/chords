module Click exposing (Click, add, keepBefore, countStarted)

import IdChord exposing (IdChord)

type alias Click =
  { idChord : IdChord
  , time : Float
  }

add : Click -> List Click -> List Click
add click clicks =
  click :: keepBefore click.time clicks

keepBefore : Float -> List Click -> List Click
keepBefore time clicks =
  case clicks of
    [] ->
      []
    click :: rest ->
      if click.time >= time then
        keepBefore time rest
      else
        clicks

countStarted : List Click -> Float -> Int
countStarted clicks time =
  List.length (dropAfter time clicks)

dropAfter : Float -> List Click -> List Click
dropAfter time clicks =
  case clicks of
    [] ->
      []
    click :: rest ->
      if click.time > time then
        dropAfter time rest
      else
        clicks
{-
toBeat : Bool -> Metro -> Click -> Click
toBeat quantize metro click =
  { click
  | time =
      if quantize then

      metro.toBeat
  }
  -}