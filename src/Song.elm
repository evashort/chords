module Song exposing (Song, view)

import IdChord exposing (IdChord, PlayStatus)

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
        , ( "font-size", "150%" )
        ]
    ]
    ( List.concatMap
        List.concat
        (indexedMap2d (viewCell tonic playStatus) song)
    )

indexedMap2d : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2d f rows =
  List.indexedMap (List.indexedMap << f) rows

viewCell :
  Int -> PlayStatus -> Int -> Int -> Maybe IdChord -> List (Html IdChord.Msg)
viewCell tonic playStatus y x cell =
  Maybe.withDefault
    [ span [] [] ]
    (Maybe.map (IdChord.view tonic playStatus y x) cell)
