module History exposing (History, add, finishSequence, view)

import CachedChord exposing (CachedChord)

import Html exposing (Html, div, mark, span, text)
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
  if List.length history.current > 1 then
    { sequences = List.take 10 (history.current :: history.sequences)
    , current = []
    }
  else
    { history | current = [] }

view : Int -> Int -> List (List CachedChord) -> Html msg
view key lowestNote sequences =
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
        (List.map (viewSequence key lowestNote) sequences)
    ]

viewSequence : Int -> Int -> List CachedChord -> Html msg
viewSequence key lowestNote sequence =
  div []
    ( List.intersperse
        (text " ")
        (List.map (viewChord key lowestNote) (List.reverse sequence))
    )

viewChord : Int -> Int -> CachedChord -> Html msg
viewChord key lowestNote cache =
  mark
    [ style
        [ ( "background", CachedChord.bg key cache )
        , ( "color", CachedChord.fg cache )
        , ( "border-radius", "3px" )
        ]
    ]
    [ text (CachedChord.codeName lowestNote cache)
    ]
