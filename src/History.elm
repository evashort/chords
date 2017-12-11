module History exposing (History, add, finishSequence, view)

import CachedChord exposing (CachedChord)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

type alias History =
  { sequences : List (List CachedChord)
  , current : List CachedChord
  }

add : CachedChord -> History -> History
add cache history =
  case history.current of
    [] ->
      { history | current = [ cache ] }
    lastCache :: _ ->
      if cache.chord == lastCache.chord then
        history
      else
        { history | current = cache :: history.current }

finishSequence : History -> History
finishSequence history =
  { sequences = List.take 10 (history.current :: history.sequences)
  , current = []
  }

view : List (List CachedChord) -> Html msg
view sequences =
  span
    [ style
        [ ( "display", "inline-block" )
        , ( "vertical-align", "top" )
        ]
    ]
    [ div []
        [ text "Recently played"
        ]
    , div
        [ style
            [ ( "font-family", "\"Lucida Console\", Monaco, monospace" )
            , ( "font-size", "13pt" )
            ]
        ]
        (List.map viewSequence sequences)
    ]

viewSequence : List CachedChord -> Html msg
viewSequence sequence =
  div []
    [ text
        (String.join " " (List.map .codeName (List.reverse sequence)))
    ]
