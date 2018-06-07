module History exposing (History, add, finishSequence, view)

import Chord exposing (Chord)
import Colour
import Name

import Html exposing (Html, div, mark, span, text)
import Html.Attributes exposing (style)

type alias History =
  { sequences : List (List Chord)
  , current : List Chord
  }

add : Chord -> History -> History
add chord history =
  case history.current of
    [] ->
      { history | current = [ chord ] }
    lastChord :: _ ->
      if chord == lastChord then
        history
      else
        { history | current = chord :: history.current }

finishSequence : History -> History
finishSequence history =
  if List.length history.current > 1 then
    { sequences = List.take 10 (history.current :: history.sequences)
    , current = []
    }
  else
    { history | current = [] }

view : Int -> List (List Chord) -> Html msg
view key sequences =
  span
    [ style
        [ ( "display", "inline-block" )
        , ( "vertical-align", "top" )
        ]
    ]
    [ div []
        [ text "Recently played"
        ]
    , div
        [ style
            [ ( "font-family", "\"Lucida Console\", Monaco, monospace" )
            , ( "font-size", "13pt" )
            ]
        ]
        (List.map (viewSequence key) sequences)
    ]

viewSequence : Int -> List Chord -> Html msg
viewSequence key sequence =
  div []
    ( List.intersperse
        (text " ")
        (List.map (viewChord key) (List.reverse sequence))
    )

viewChord : Int -> Chord -> Html msg
viewChord key chord =
  mark
    [ style
        [ ( "background", Colour.swatchBg key chord )
        , ( "color", Colour.fg chord )
        , ( "border-radius", "3px" )
        ]
    ]
    [ text (Name.code chord)
    ]
