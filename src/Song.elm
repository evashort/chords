module Song exposing (Song, view)

import IdChord exposing (IdChord)
import PlayStatus exposing (PlayStatus)

import Html exposing (Html, span)
import Html.Attributes exposing (style)

type alias Song = List (List (Maybe IdChord))

view : String -> Int -> PlayStatus -> Song -> Html IdChord.Msg
view gridArea tonic playStatus song =
  span
    [ style
        [ ( "grid-area", gridArea )
        , ( "display", "grid" )
        , ( "position", "relative" )
        , ( "grid-auto-rows", "75px" )
        , ( "grid-auto-columns", "75px" )
        , ( "grid-row-gap", "5px" )
        , ( "grid-column-gap", "5px" )
        , ( "align-items", "stretch" )
        , ( "justify-items", "stretch" )
        , ( "font-size", "150%" )
        , ( "margin-top", "5px" )
        ]
    ]
    ( List.concat
        (indexedMap2d (viewCell tonic playStatus) song)
    )

indexedMap2d : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2d f rows =
  List.indexedMap (List.indexedMap << f) rows

viewCell :
  Int -> PlayStatus -> Int -> Int -> Maybe IdChord -> Html IdChord.Msg
viewCell tonic playStatus y x cell =
  span
    [ style
        [ ( "grid-row-start", toString (y + 1) )
        , ( "grid-column-start", toString (x + 1) )
        , ( "grid-row-end", "span 1" )
        , ( "grid-column-end", "span 1" )
        , ( "position", "relative" )
        ]
    ]
    ( case cell of
        Nothing ->
          []
        Just idChord ->
          IdChord.view tonic playStatus idChord
    )
