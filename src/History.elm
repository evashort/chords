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
  , shortenSequences : Bool
  }

init : History
init =
  { sequences = []
  , length = 0
  , shortenSequences = True
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
view gridArea tonic history sequence finished =
  span
    [ style
        [ ( "grid-area", gridArea )
        , ( "display", "grid" )
        , ( "grid-template-columns", "auto 1fr" )
        ]
    ]
    ( List.concat
        [ if List.length sequence >= 2 then
            viewSequence
              history.shortenSequences
              finished
              tonic
              history.length
              sequence
          else
            []
        , List.concat
            ( List.indexedMap
                ( viewSequence
                    history.shortenSequences
                    True
                    tonic <<
                  (-) (history.length - 1)
                )
                history.sequences
            )
        ]
    )

viewSequence : Bool -> Bool -> Int -> Int -> List Chord -> List (Html String)
viewSequence shorten finished tonic index sequence =
  let
    shortenedSequence =
      if shorten then
          List.drop
            (List.length sequence - 8)
            sequence
        else
          sequence
  in
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
            , onClick
                ( String.join
                    " "
                    (List.map Name.code shortenedSequence)
                )
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
            [ if shorten && List.length sequence > 8 then
                [ text "..." ]
              else
                []
            , List.intersperse
                (text " ")
                ( List.map
                    (viewChord tonic)
                    shortenedSequence
                )
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
viewChord tonic chord =
  mark
    [ style
        [ ( "background", Colour.swatchBg tonic chord )
        , ( "color", Colour.fg chord )
        , ( "border-radius", "3px" )
        ]
    ]
    [ text (Name.code chord)
    ]
