module PlayBar exposing (Msg(..), view)

import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import PlayStyle
import Radio
import Selection exposing (Selection)
import Storage exposing (Storage)
import StrumPattern

import Html exposing (Html, span, div, button, text, input, canvas, label, br)
import Html.Attributes as Attributes exposing
  (id, style, class, type_, value, checked)
import Html.Events exposing (onInput, onCheck)
import Svg
import Svg.Attributes as SA

type Msg
  = SetStorage Storage
  | Play Float

view : String -> Storage -> Selection -> Html Msg
view gridArea storage selection =
  span
    [ id "playStyle"
    , style "grid-area" gridArea
    , style "position" "-webkit-sticky"
    , style "position" "sticky"
    , style "top" "0px"
    , style "z-index" "2"
    , style "justify-self" "start"
    , style "margin-left" "-8px"
    , style "padding" "8px"
    , style "background" "white"
    , style "border-bottom-right-radius" "5px"
    , style "box-shadow" "rgba(0, 0, 0, 0.5) 1px 1px 8px -1px"
    ]
    [ button
        [ style "vertical-align" "middle"
        , isAudioTimeButton True
        , onClickWithAudioTime Play
        ]
        [ text "Play"
        ]
    , text "\u{00A0}"
    , span
        [ style "display" "inline-block"
        , style "vertical-align" "middle"
        ]
        [ Html.map
            (\x -> SetStorage { storage | playStyle = x })
            ( Radio.view
                False
                storage.playStyle
                [ ( "Arpeggio", PlayStyle.Arpeggio )
                , ( "Strum", PlayStyle.StrumPattern )
                , ( "Pad", PlayStyle.Pad )
                ]
            )
        , text "\u{00A0}Vol.\u{00A0}"
        , span
            [ style "position" "relative"
            , style "display" "inline-block"
            , style "vertical-align" "middle"
            ]
            [ input
                [ type_ "range"
                , class "range"
                , onInput (SetStorage << Storage.setVolume storage)
                , value (String.fromInt storage.volume)
                , Attributes.min "0"
                , Attributes.max "30"
                , style "width" "8em"
                , style "display" "block"
                , style "padding-bottom" "0"
                ]
                []
            , canvas
                [ style "position" "absolute"
                , style "left" "6px"
                , style "width" "calc(100% - 12px)"
                , style "top" "100%"
                , style "height" "1em"
                , Attributes.width 100
                , Attributes.height 11
                , id "meter"
                ]
                []
            ]
        , br [] []
        , if storage.playStyle == PlayStyle.StrumPattern then
            span
              [ style "position" "relative"
              ]
              [ Svg.svg
                  [ style "position" "absolute"
                  , style "bottom" "100%"
                  , style "left" "0%"
                  , SA.width "150"
                  , SA.height "15"
                  , SA.viewBox "0 0 150 15"
                  , style "pointer-events" "none"
                  ]
                  [ Svg.path
                      [ SA.stroke "gray"
                      , SA.fill "rgb(194, 230, 252)"
                      , SA.strokeWidth "1"
                      , SA.d "M2,12 L70.5,5 L121.5,5 L148,12"
                      ]
                      []
                  ]
              , Html.map
                  (\x -> SetStorage { storage | strumPattern = x })
                  ( Radio.view
                      False
                      storage.strumPattern
                      [ ( "Basic", StrumPattern.Basic )
                      , ( "Indie", StrumPattern.Indie )
                      , ( "Modern", StrumPattern.Modern )
                      ]
                  )
              ]
          else
            text "\u{00A0}"
        ]
    , br [] []
    , label
        [ class "checkboxLabel"
        ]
        [ input
            [ type_ "checkbox"
            , checked storage.strumOnSelect
            , onCheck (\x -> SetStorage { storage | strumOnSelect = x })
            ]
            []
        , Html.text "\u{00A0}Strum on select\u{00A0}"
        ]
    , input
        [ type_ "range"
        , class "range"
        , onInput (SetStorage << Storage.setStrumInterval storage)
        , Attributes.min "10"
        , Attributes.max "90"
        , Attributes.step "20"
        , value (String.fromFloat (1000 * storage.strumInterval))
        , style "width" "5em"
        , style "vertical-align" "middle"
        ]
        []
    , Html.text
        ( String.concat
            [ "\u{00A0}"
            , String.fromFloat (1000 * storage.strumInterval)
            , "ms between notes"
            ]
        )
    ]
