module Radio exposing (view)

import Html exposing (Html, span, button, text)
import Html.Attributes as Attributes exposing (style, class, classList)
import Html.Events exposing (onClick)

view : a -> List (String, a) -> Html a
view chosen options =
  span
    [ class "radio"
    , style
        [ ( "display", "inline-grid" )
        , ( "grid-template-columns"
          , String.concat
              [ "repeat("
              , toString (List.length options - 1)
              , ", auto 1px) auto"
              ]
          )
        , ( "position", "relative" )
        ]
    ]
    ( List.indexedMap
        (viewOption chosen (List.length options))
        options
    )

viewOption : a -> Int -> Int -> (String, a) -> Html a
viewOption chosen length i ( label, value ) =
  button
    [ onClick value
    , classList [ ( "chosen", chosen == value ) ]
    , style
        [ ( "grid-column-start", toString (max 1 (2 * i)) )
        , ( "grid-column-end", toString (min (2 * length) (2 * i + 3)) )
        , ( "grid-row", "1" )
        ]
    ]
    [ text label
    , span [ class "hover" ] []
    , span [ class "active" ] []
    ]
