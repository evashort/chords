module ChordsInKey exposing (Msg(..), view)

import Chord exposing (Chord)
import ChordView
import IdChord exposing (IdChord)
import Player exposing (Player)
import PlayStyle exposing (PlayStyle)
import Radio
import Scale exposing (Scale)
import Storage exposing (Storage)

import Html exposing (Html, span, text, sup, label, input)
import Html.Attributes exposing (style, class, id, type_, checked)
import Html.Events exposing (onCheck)

type Msg
  = SetStorage Storage
  | ChordViewMsg ChordView.Msg

view : Storage -> Scale -> Player -> Maybe IdChord -> Html Msg
view storage scale player selection =
  span
    [ id "chordsInKeyPane"
    , style "display" "block"
    ]
    [ span
        [ style "display" "block"
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
        player
        selection
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

viewGrid : Scale -> Player -> Maybe IdChord -> List String -> Html Msg
viewGrid scale player selection rows =
  span
    [ style "display" "inline-grid"
    , style "grid-template-rows" "auto"
    , style "grid-auto-rows" "75px"
    , style "grid-template-columns" "auto"
    , style "grid-auto-columns" "75px"
    , style "grid-row-gap" "5px"
    , style "grid-column-gap" "5px"
    ]
    ( case rows of
        [] ->
          []
        header :: rest ->
          (++)
            (List.indexedMap viewDegree (split header))
            ( List.concat
                ( List.indexedMap
                    (viewRow scale player selection)
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
    [ style "grid-row" "1"
    , style "grid-column" (String.fromInt (i + 2))
    , style "align-self" "baseline"
    , style "justify-self" "center"
    , style "font-size" "150%"
    , style "line-height" "initial"
    ]
    ( if String.endsWith "o" name then
        [ text (String.dropRight 1 name)
        , sup [] [ text "o" ]
        ]
      else
        [ text name
        ]
    )

viewRow : Scale -> Player -> Maybe IdChord -> Int -> String -> List (Html Msg)
viewRow scale player selection i row =
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
          ( indexedMaybeMap
              (viewCell scale.tonic player selection i)
              (List.map Chord.fromCode cells)
          )

indexedMaybeMap : (Int -> a -> b) -> List (Maybe a) -> List b
indexedMaybeMap f xs =
  List.filterMap identity (List.indexedMap (indexedMaybeMapHelp f) xs)

indexedMaybeMapHelp : (Int -> a -> b) -> Int -> Maybe a -> Maybe b
indexedMaybeMapHelp f i x =
  Maybe.map (f i) x

viewCategory : Int -> String -> Html msg
viewCategory i name =
  span
    [ style "grid-column" "1"
    , style "grid-row" (String.fromInt (i + 2))
    , style "align-self" "center"
    , style "justify-self" "center"
    , style "font-size" "initial"
    ]
    ( if String.endsWith "th" name then
        [ text (String.dropRight 2 name)
        , sup [] [ text "th" ]
        ]
      else
        [ text name
        ]
    )

viewCell :
  Int -> Player -> Maybe IdChord -> Int -> Int -> Chord -> Html Msg
viewCell tonic player selection y x chord =
  let
    idChord =
      case
        IdChord.fromChord (Chord.transpose tonic chord)
      of
        Nothing ->
          Debug.todo
            ( "ChordsInKey.viewCell: Unknown chord " ++
                Debug.toString chord
            )
        Just something ->
          something
  in
    Html.map
      ChordViewMsg
      ( span
          [ style "grid-row-start" (String.fromInt (y + 2))
          , style "grid-column-start" (String.fromInt (x + 2))
          , style "position" "relative"
          , style "font-size" "150%"
          ]
          (ChordView.view tonic player selection idChord)
      )
