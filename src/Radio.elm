module Radio exposing (view)

import Html exposing (Html, span, button, text)
import Html.Attributes as Attributes exposing (style, class, classList)
import Html.Events exposing (onClick)

view : Bool -> a -> List (String, a) -> Html a
view disabled chosen options =
  span
    [ class "radio"
    , style "display" "inline-grid"
    , style
        "grid-template-columns"
        ( String.concat
            [ "repeat("
            , String.fromInt (List.length options - 1)
            , ", auto 1px) auto"
            ]
        )
    , style "position" "relative"
    ]
    ( List.indexedMap
        (viewOption disabled chosen (List.length options))
        options
    )

viewOption : Bool -> a -> Int -> Int -> (String, a) -> Html a
viewOption disabled chosen length i ( label, value ) =
  button
    [ Attributes.disabled (disabled && chosen /= value)
    , onClick value
    , classList [ ( "chosen", chosen == value ) ]
    , style
        "grid-column-start"
        (String.fromInt (max 1 (2 * i)))
    , style
        "grid-column-end"
        (String.fromInt (min (2 * length) (2 * i + 3)))
    , style "grid-row" "1"
    ]
    [ text label
    , span [ class "hover" ] []
    , span [ class "active" ] []
    ]
