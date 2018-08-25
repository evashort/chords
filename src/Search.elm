module Search exposing (Msg(..), view)

import Chord exposing (Chord)
import Chroma
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import IdChord exposing (IdChord)
import PlayStatus exposing (PlayStatus)

import Html exposing (Html, span, button)
import Html.Attributes exposing (style, classList, id, disabled)

type Msg
  = ShowCustomChord (Bool, Float)
  | IdChordMsg IdChord.Msg

view : Int -> Bool -> String -> PlayStatus -> Html Msg
view tonic showCustomChord customCode playStatus =
  let
    maybeChord = Chord.fromCodeExtended customCode
  in let
    exactMatch =
      Maybe.andThen IdChord.fromChord maybeChord
    inversions =
      case maybeChord of
        Nothing ->
          []
        Just chord ->
          Chroma.search chord
  in let
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
      , style
          [ ( "display", "flex" )
          , ( "align-items", "flex-end" )
          ]
      ]
      ( List.concat
          [ [ span
                [ style
                    [ ( "margin-right", "5px" )
                    , ( "margin-bottom", "5px" )
                    ]
                ]
                [ Html.br [] []
                , viewCustomChord
                    tonic
                    showCustomChord
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
                    playStatus
                    ( "Exact match", idChord )
                ]
          , List.map
              (viewSearchResult tonic playStatus)
              inversions
          , List.map
              (viewSearchResult tonic playStatus)
              extensions
          , List.map
              (viewSearchResult tonic playStatus)
              subsets
          ]
      )

viewCustomChord : Int -> Bool -> Bool -> Chord -> Html Msg
viewCustomChord tonic showCustomChord noCustomChord colorChord =
  let
    action =
      ShowCustomChord << (,) (not showCustomChord)
  in
    span
      [ style
          [ ( "width", "75px" )
          , ( "height", "75px" )
          , ( "position", "relative" )
          , ( "display", "inline-block" )
          ]
      ]
      [ span
          [ classList
              [ ( "chordBorder", True )
              , ( "hasBorder", showCustomChord )
              ]
          , style
              [ ( "position", "absolute" )
              , ( "top", "-5px" )
              , ( "left", "-5px" )
              , ( "right", "-5px" )
              , ( "bottom", "-5px" )
              , ( "pointer-events", "none" )
              , ( "border-width", "5px" )
              , ( "border-radius", "10px" )
              , ( "border-color"
                , if showCustomChord then
                    "#3399ff"
                  else
                    "transparent"
                )
              , ( "border-style", "solid" )
              ]
          ]
          []
      , button
          [ isAudioTimeButton True
          , onClickWithAudioTime action
          , disabled noCustomChord
          , style
              [ ( "width", "100%" )
              , ( "height", "100%" )
              , ( "background", Colour.bg tonic colorChord )
              , ( "color"
                , if noCustomChord then
                    ""
                  else
                    Colour.fg colorChord
                )
              , ( "padding", "0" )
              , ( "border"
                , String.concat
                    [ "1px solid rgba(0, 0, 0, "
                    , Colour.borderOpacity colorChord
                    , ")"
                    ]
                )
              , ( "border-radius", "5px" )
              , ( "box-shadow"
                , String.concat
                    [ "inset 18px 34px 20px -20px rgba(255, 255, 255, "
                    , Colour.shineOpacity colorChord
                    , ")"
                    ]
                )
              , ( "cursor"
                , if noCustomChord then
                    ""
                  else
                    "pointer"
                )
              , ( "white-space", "normal" )
              ]
          ]
          [ Html.text "Show custom chord"
          ]
      ]

viewSearchResult : Int -> PlayStatus -> (String, IdChord) -> Html Msg
viewSearchResult tonic playStatus ( header, idChord ) =
  Html.map
    IdChordMsg
    ( span
        [ style
            [ ( "text-align", "center" )
            , ( "margin-right", "5px" )
            , ( "margin-bottom", "5px" )
            ]
        ]
        [ Html.text header
        , Html.br [] []
        , span
            [ style
                [ ( "width", "75px" )
                , ( "height", "75px" )
                , ( "position", "relative" )
                , ( "display", "inline-block" )
                , ( "font-size", "150%" )
                ]
            ]
            (IdChord.view tonic playStatus idChord)
        ]
    )
