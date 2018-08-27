module History exposing (History, init, add, Msg(..), view)

import Chord exposing (Chord)
import Colour
import Name
import Storage exposing (Storage)

import Html exposing (Html, span, button, text, mark, label, input)
import Html.Attributes exposing (style, class, id, disabled, type_, checked)
import Html.Events exposing (onClick, onCheck)

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

type Msg
  = SetStorage Storage
  | AddLine String

view : Int -> Storage -> History -> List Chord -> Bool -> Html Msg
view tonic storage history sequence finished =
  span
    [ id "historyPane"
    , style "display" "block"
    ]
    [ label
        [ class "checkboxLabel"
        ]
        [ input
            [ type_ "checkbox"
            , checked storage.shortenSequences
            , onCheck
                ( \x ->
                    SetStorage { storage | shortenSequences = x }
                )
            ]
            []
        , Html.text " Show only last 8 chords of each sequence"
        ]
    , if List.length sequence >= 2 || history.sequences /= [] then
        viewNonEmpty tonic storage.shortenSequences history sequence finished
      else
        span
          [ style "display" "block"
          , style "color" "GrayText"
          , style "background" (indexBackground 0)
          , style "padding" "calc(0.22em + 5px) 10px"
          , style "white-space" "initial"
          , style "line-height" "initial"
          ]
          [ text
              "Sequences of two or more chords played consecutively will appear here."
          ]
    ]

viewNonEmpty : Int -> Bool -> History -> List Chord -> Bool -> Html Msg
viewNonEmpty tonic shorten history sequence finished =
  span
    [ style "display" "grid"
    , style "grid-template-columns" "auto 1fr"
    ]
    ( List.concat
        [ if List.length sequence >= 2 then
            viewSequence
              shorten
              finished
              tonic
              history.length
              sequence
          else
            []
        , List.concat
            ( List.indexedMap
                ( viewSequence shorten True tonic <<
                    (-) (history.length - 1)
                )
                history.sequences
            )
        ]
    )

viewSequence : Bool -> Bool -> Int -> Int -> List Chord -> List (Html Msg)
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
        [ style "background" (indexBackground index)
        , style "grid-column" "1"
        , style "padding" "5px"
        , style "padding-right" "10px"
        , style "display" "flex"
        , style "align-items" "center"
        ]
        [ button
            [ class "button"
            , disabled (not finished)
            , onClick
                ( AddLine
                    ( String.join
                        " "
                        (List.map Name.code shortenedSequence)
                    )
                )
            ]
            [ text "Add line"
            ]
        ]
    , span
        [ style "background" (indexBackground index)
        , style "font-family" "\"Lucida Console\", Monaco, monospace"
        , style "font-size" "160%"
        , style "line-height" "initial"
        , style "white-space" "initial"
        , style "padding" "5px"
        , style "padding-left" "0px"
        , style "grid-column" "2"
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
  if modBy 2 index == 1 then
    "#ffffff"
  else
    "#eeeeee"

viewChord : Int -> Chord -> Html msg
viewChord tonic chord =
  mark
    [ style "background" (Colour.swatchBg tonic chord)
    , style "color" (Colour.fg chord)
    , style "border-radius" "3px"
    ]
    [ text (Name.code chord)
    ]
