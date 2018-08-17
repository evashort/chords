module ChordsInKey exposing (Msg(..), view)

import Chord exposing (Chord)
import IdChord
import PlayStatus exposing (PlayStatus)
import Radio
import Scale exposing (Scale)
import Storage exposing (Storage)

import Html exposing (Html, span, text, sup, label, input)
import Html.Attributes exposing (style, class, id, type_, checked)
import Html.Events exposing (onCheck)

type Msg
  = SetStorage Storage
  | IdChordMsg IdChord.Msg

view : Storage -> Scale -> PlayStatus -> Html Msg
view storage scale playStatus =
  span
    [ id "chordsInKeyPane"
    , style
        [ ( "display", "block" )
        ]
    ]
    [ span
        [ style
            [ ( "display", "block" )
            ]
        ]
        [ Html.map
            (\x -> SetStorage { storage | addedToneChords = x })
            ( Radio.view
                False
                storage.addedToneChords
                [ ( "Extended chords", False )
                , ( "Added tone chords", True )
                ]
            )
        , Html.text " "
        , label
            [ class "checkboxLabel"
            ]
            [ input
                [ type_ "checkbox"
                , checked storage.harmonicMinor
                , onCheck
                    (\x -> SetStorage { storage | harmonicMinor = x })
                ]
                []
            , Html.text " Chords from harmonic minor"
            ]
        ]
    , viewGrid
        scale
        playStatus
        [ case ( scale.minor, storage.harmonicMinor ) of
            ( False, False ) ->
              "I ii iii IV V vi viio"
            ( False, True ) ->
              "I ii III IV V vi viio"
            ( True, False ) ->
              "i iio III iv v VI VII"
            ( True, True ) ->
              "i iio III iv V VI VII"
        , if storage.harmonicMinor then
            "Triad C Dm E F G Am Bo"
          else
            "Triad C Dm Em F G Am Bo"
        , if storage.harmonicMinor then
            "7th CM7 Dm7 E7 FM7 G7 AmM7 Bo7"
          else
            "7th CM7 Dm7 Em7 FM7 G7 Am7 B0"
        , case
            ( storage.addedToneChords, storage.harmonicMinor )
          of
            ( False, False ) ->
              "9th CM9 Dm9 _ FM9 G9 Am9 _"
            ( False, True ) ->
              "9th CM9 Dm9 E7b9 FM9 G9 Am9 _"
            ( True, False ) ->
              "9th Cadd9 Dmadd9 _ Fadd9 Gadd9 Amadd9 _"
            ( True, True ) ->
              "9th Cadd9 Dmadd9 E7addb9 Fadd9 Gadd9 Amadd9 _"
        , if storage.addedToneChords then
            "11th _ _ _ Fadd#11 _ _ _"
          else
            "11th _ _ _ FM7#11 _ _ _"
        , if storage.addedToneChords then
            "13th C6 Dm6 _ F6 G6 _ _"
          else
            "13th CM13 _ _ FM13 G13 _ _"
        ]
    ]

viewGrid : Scale -> PlayStatus -> List String -> Html Msg
viewGrid scale playStatus rows =
  span
    [ style
        [ ( "display", "inline-grid" )
        , ( "grid-template-rows", "auto" )
        , ( "grid-auto-rows", "75px" )
        , ( "grid-template-columns", "auto" )
        , ( "grid-auto-columns", "75px" )
        , ( "grid-row-gap", "5px" )
        , ( "grid-column-gap", "5px" )
        ]
    ]
    ( case rows of
        [] ->
          []
        header :: rest ->
          (++)
            (List.indexedMap viewDegree (split header))
            ( List.concat
                ( List.indexedMap
                    (viewRow scale playStatus)
                    rest
                )
            )
    )

split : String -> List String
split string =
  List.filter
    (not << String.isEmpty)
    (String.split " " string)

viewDegree : Int -> String -> Html msg
viewDegree i name =
  span
    [ style
        [ ( "grid-row", "1" )
        , ( "grid-column", toString (i + 2) )
        , ( "align-self", "baseline" )
        , ( "justify-self", "center" )
        , ( "font-size", "150%" )
        , ( "line-height", "initial" )
        ]
    ]
    ( if String.endsWith "o" name then
        [ text (String.dropRight 1 name)
        , sup [] [ text "o" ]
        ]
      else
        [ text name
        ]
    )

viewRow : Scale -> PlayStatus -> Int -> String -> List (Html Msg)
viewRow scale playStatus i row =
  case split row of
    [] ->
      []
    category :: majorCells ->
      let
        cells =
          if scale.minor then
            List.drop 5 majorCells ++ List.take 5 majorCells
          else
            majorCells
      in
        (::)
          (viewCategory i category)
          (List.indexedMap (viewCell scale.tonic playStatus i) cells)

viewCategory : Int -> String -> Html msg
viewCategory i name =
  span
    [ style
        [ ( "grid-column", "1" )
        , ( "grid-row", toString (i + 2) )
        , ( "align-self", "center" )
        , ( "justify-self", "center" )
        , ( "font-size", "initial" )
        ]
    ]
    ( if String.endsWith "th" name then
        [ text (String.dropRight 2 name)
        , sup [] [ text "th" ]
        ]
      else
        [ text name
        ]
    )

viewCell : Int -> PlayStatus -> Int -> Int -> String -> Html Msg
viewCell tonic playStatus y x code =
  case Chord.fromCode code of
    Nothing ->
      span
        [ style
            [ ( "grid-row-start", toString (y + 2) )
            , ( "grid-column-start", toString (x + 2) )
            ]
        ]
        []
    Just chord ->
      let
        idChord =
          case
            IdChord.fromChord (Chord.transpose tonic chord)
          of
            Nothing ->
              Debug.crash
                ("ChordsInKey.viewCell: Unknown chord " ++ toString chord)
            Just something ->
              something
      in
        Html.map
          IdChordMsg
          ( span
              [ style
                  [ ( "grid-row-start", toString (y + 2) )
                  , ( "grid-column-start", toString (x + 2) )
                  , ( "position", "relative" )
                  , ( "font-size", "150%" )
                  ]
              ]
              (IdChord.view tonic playStatus idChord)
          )
