module Song exposing (Song, view)

import IdChord exposing (IdChord)
import PlayStatus exposing (PlayStatus)

import Html exposing (Html, span)
import Html.Attributes exposing (style, id)
import Html.Keyed
import Html.Lazy

type alias Song = List (List (Maybe IdChord))

view : String -> Int -> PlayStatus -> Song -> Html IdChord.Msg
view gridArea tonic playStatus song =
  Html.Keyed.node
    "span"
    [ id gridArea
    , style "grid-area" gridArea
    , style "display" "grid"
    , style "position" "relative"
    , style "grid-auto-rows" "75px"
    , style "grid-auto-columns" "75px"
    , style "grid-row-gap" "5px"
    , style "grid-column-gap" "5px"
    , style "align-items" "stretch"
    , style "justify-items" "stretch"
    , style "font-size" "150%"
    , style "margin-top" "5px"
    ]
    ( List.concat
        (indexedMap2d (viewCell tonic playStatus) song)
    )

indexedMap2d : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2d f rows =
  List.indexedMap (List.indexedMap << f) rows

viewCell :
  Int -> PlayStatus -> Int -> Int -> Maybe IdChord ->
    (String, Html IdChord.Msg)
viewCell tonic playStatus y x cell =
  ( case cell of
      Nothing ->
        "space" ++ String.fromInt x ++ "_" ++ String.fromInt y
      Just idChord ->
        String.fromInt idChord.id
  , span
      [ style "grid-row-start" (String.fromInt (y + 1))
      , style "grid-column-start" (String.fromInt (x + 1))
      , style "grid-row-end" "span 1"
      , style "grid-column-end" "span 1"
      ]
      ( case cell of
          Nothing ->
            []
          Just idChord ->
            [ Html.Lazy.lazy3
                viewIdChord
                tonic
                (PlayStatus.isolate idChord.id playStatus)
                idChord
            ]
      )
  )

viewIdChord : Int -> PlayStatus -> IdChord -> Html IdChord.Msg
viewIdChord tonic playStatus idChord =
  span
    [ style "display" "block"
    , style "position" "relative"
    , style "width" "100%"
    , style "height" "100%"
    ]
    (IdChord.view tonic playStatus idChord)
