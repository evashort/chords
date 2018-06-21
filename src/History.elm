module History exposing (History, init, add, view)

import Chord exposing (Chord)
import Colour
import Name

import Html exposing (Html, span, button, text, mark)
import Html.Attributes exposing (style, disabled)
import Html.Events exposing (onClick)

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

view : String -> Int -> History -> List Chord -> Bool -> Html String
view gridArea key history sequence finished =
  span
    [ style
        [ ( "grid-area", gridArea )
        , ( "display", "grid" )
        , ( "grid-template-columns", "auto 1fr" )
        , ( "align-items", "stretch" )
        ]
    ]
    ( List.concat
        [ if List.length sequence >= 2 then
            viewSequence finished key history.length sequence
          else
            []
        , List.concat
            ( List.indexedMap
                (viewSequence True key << (-) (history.length - 1))
                history.sequences
            )
        ]
    )

viewSequence : Bool -> Int -> Int -> List Chord -> List (Html String)
viewSequence finished key index sequence =
  [ span
      [ style
          [ ( "background", indexBackground index )
          , ( "grid-column", "1" )
          , ( "padding", "5px" )
          , ( "padding-right", "10px" )
          , ( "display", "flex" )
          , ( "align-items", "center" )
          ]
      ]
      [ button
          [ disabled (not finished)
          , onClick (String.join " " (List.map Name.code sequence))
          ]
          [ text "Use"
          ]
      ]
  , span
      [ style
          [ ( "background", indexBackground index )
          , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
          , ( "font-size", "160%" )
          , ( "line-height", "initial" )
          , ( "white-space", "initial" )
          , ( "padding", "5px" )
          , ( "padding-left", "0px" )
          , ( "grid-column", "2" )
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
  ]

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
