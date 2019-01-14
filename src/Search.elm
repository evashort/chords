module Search exposing (Msg(..), view)

import Chord exposing (Chord)
import ChordView
import Chroma
import Click exposing (Click)
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import IdChord exposing (IdChord)
import Selection exposing (Selection)

import Html exposing (Html, span, button)
import Html.Attributes exposing (style, classList, id, disabled)

type Msg
  = CustomClicked Float
  | Clicked Click

view : Int -> String -> Selection -> Html Msg
view tonic customCode selection =
  let
    maybeChord = Chord.fromCodeExtended customCode
  in
  let
    exactMatch =
      Maybe.andThen IdChord.fromChord maybeChord
    inversions =
      case maybeChord of
        Nothing ->
          []
        Just chord ->
          Chroma.search chord
  in
  let
    extensions =
      case ( maybeChord, exactMatch, inversions ) of
        ( Just chord, Nothing, [] ) ->
          Chroma.extendedSearch chord
        _ ->
          []
    subsets =
      case ( maybeChord, exactMatch, inversions ) of
        ( Just chord, Nothing, [] ) ->
          Chroma.subsetSearch tonic chord
        _ ->
          []
    colorChord =
      case ( exactMatch, inversions ) of
        ( Just match, _ ) ->
          match.chord
        ( _, ( _, inversion ) :: _ ) ->
          inversion.chord
        _ ->
          Chord [] 0
  in
    span
      [ id "searchPane"
      , style "display" "flex"
      , style "align-items" "flex-end"
      ]
      ( List.concat
          [ [ span
                [ style "margin-right" "5px"
                , style "margin-bottom" "5px"
                ]
                [ Html.br [] []
                , viewCustomChord
                    tonic
                    (selection == Selection.Custom)
                    (String.isEmpty customCode)
                    colorChord
                ]
            ]
          , case exactMatch of
              Nothing ->
                []
              Just idChord ->
                [ viewSearchResult
                    tonic
                    selection
                    ( "Exact match", idChord )
                ]
          , List.map
              (viewSearchResult tonic selection)
              inversions
          , List.map
              (viewSearchResult tonic selection)
              extensions
          , List.map
              (viewSearchResult tonic selection)
              subsets
          ]
      )

viewCustomChord : Int -> Bool -> Bool -> Chord -> Html Selection.Msg
viewCustomChord tonic showCustomChord noCustomChord colorChord =
  let action = Selection.CustomClicked in
    span
      [ style "position" "relative"
      , style "display" "inline-block"
      ]
      [ span
          [ classList
              [ ( "chordBorder", True )
              , ( "hasBorder", showCustomChord )
              ]
          , style "position" "absolute"
          , style "top" "-5px"
          , style "left" "-5px"
          , style "right" "-5px"
          , style "bottom" "-5px"
          , style "pointer-events" "none"
          , style "border-width" "5px"
          , style "border-radius" "10px"
          , style
              "border-color"
              ( if showCustomChord then
                  "#3399ff"
                else
                  "transparent"
              )
          , style "border-style" "solid"
          ]
          []
      , button
          [ isAudioTimeButton True
          , onClickWithAudioTime action
          , disabled noCustomChord
          , style "width" "76.8px"
          , style "height" "76.8px"
          , style "background" (Colour.bg tonic colorChord)
          , style
              "color"
              ( if noCustomChord then
                  ""
                else
                  Colour.fg colorChord
              )
          , style "padding" "0"
          , style
              "border"
              ( String.concat
                  [ "1px solid rgba(0, 0, 0, "
                  , Colour.borderOpacity colorChord
                  , ")"
                  ]
              )
          , style "border-radius" "5px"
          , style
              "box-shadow"
              ( String.concat
                  [ "inset 18px 34px 20px -20px rgba(255, 255, 255, "
                  , Colour.shineOpacity colorChord
                  , ")"
                  ]
              )
          , style
              "cursor"
              ( if noCustomChord then
                  ""
                else
                  "pointer"
              )
          , style "white-space" "normal"
          ]
          [ Html.text "Show custom chord"
          ]
      ]

viewSearchResult : Int -> Selection -> (String, IdChord) -> Html Msg
viewSearchResult tonic selection ( header, idChord ) =
  span
    [ style "text-align" "center"
    , style "margin-right" "5px"
    , style "margin-bottom" "5px"
    ]
    [ Html.text header
    , Html.br [] []
    , span
        [ style "position" "relative"
        , style "display" "inline-block"
        , style "font-size" "150%"
        ]
        (Html.map Clicked (ChordView.view tonic selection idChord))
    ]
