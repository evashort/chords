module Song exposing (Song, view)

import ChordView
import Click exposing (Click)
import IdChord exposing (IdChord)
import Selection exposing (Selection)

import Html exposing (Html, span)
import Html.Attributes exposing (style, id)
import Html.Keyed

type alias Song = List (List (Maybe IdChord))

view : String -> Int -> Selection -> Song -> Html Click
view gridArea tonic selection song =
  Html.Keyed.node
    "span"
    [ id gridArea
    , style "grid-area" gridArea
    , style "display" "grid"
    , style "position" "relative"
    , style "grid-auto-rows" "3.2em"
    , style "grid-auto-columns" "3.2em"
    , style "grid-row-gap" "5px"
    , style "grid-column-gap" "5px"
    , style "align-items" "stretch"
    , style "justify-items" "stretch"
    , style "font-size" "150%"
    , style "margin-top" "5px"
    , style
        "height"
        ( String.concat
            [ "calc((3.2em + 5px) * "
            , String.fromInt (List.length song)
            , " - 5px)"
            ]
        )
    ]
    ( List.concat
        (List.indexedMap (viewRow tonic selection) song)
    )

viewRow :
  Int -> Selection -> Int -> List (Maybe IdChord) -> List (String, Html Click)
viewRow tonic selection y row =
  indexedMaybeMap (viewCell tonic selection y) row

indexedMaybeMap : (Int -> a -> b) -> List (Maybe a) -> List b
indexedMaybeMap f xs =
  List.filterMap identity (List.indexedMap (indexedMaybeMapHelp f) xs)

indexedMaybeMapHelp : (Int -> a -> b) -> Int -> Maybe a -> Maybe b
indexedMaybeMapHelp f i x =
  Maybe.map (f i) x

viewCell :
  Int -> Selection -> Int -> Int -> IdChord -> (String, Html Click)
viewCell tonic selection y x idChord =
  ( String.fromInt idChord.id
  , span
      [ style "grid-row-start" (String.fromInt (y + 1))
      , style "grid-column-start" (String.fromInt (x + 1))
      , style "grid-row-end" "span 1"
      , style "grid-column-end" "span 1"
      , style "position" "relative"
      ]
      (ChordView.view tonic selection idChord)
  )
