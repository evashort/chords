module History exposing (History, init, add, view)

import Chord exposing (Chord)
import Colour
import Name

import Html exposing (Html, div, mark, span, text)
import Html.Attributes exposing (style)

type alias History =
  { sequences : List (List Chord)
  , length : Int
  }

init : History
init =
  { sequences = []
  , length = 0
  }

add : List Chord -> History -> History
add sequence history =
  if List.length sequence >= 2 then
    { history
    | sequences = sequence :: history.sequences
    , length = history.length + 1
    }
  else
    history

view : String -> Int -> History -> List Chord -> Bool -> Html msg
view gridArea key history sequence finished =
  span
    [ style
        [ ( "grid-area", gridArea )
        , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
        , ( "font-size", "160%" )
        , ( "line-height", "initial" )
        , ( "white-space", "initial" )
        ]
    ]
    ( List.concat
        [ if List.length sequence >= 2 then
            [ viewSequence finished key history.length sequence ]
          else
            []
        , List.indexedMap
            (viewSequence True key << (-) (history.length - 1))
            history.sequences
        ]
    )

viewSequence : Bool -> Int -> Int -> List Chord -> Html msg
viewSequence finished key index sequence =
  div
    [ style
        [ ( "background", indexBackground index )
        , ( "padding", "5px 0px" )
        ]
    ]
    ( List.concat
        [ List.intersperse
            (text " ")
            (List.map (viewChord key) sequence)
        , if finished then
            []
          else
            [ text "..." ]
        ]
    )

indexBackground : Int -> String
indexBackground index =
  if index % 2 == 1 then
    "#ffffff"
  else
    "#eeeeee"

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
