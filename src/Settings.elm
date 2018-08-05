module Settings exposing (Msg(..), view)

import Storage exposing (Storage)

import Html exposing (Html, span, label, input, text, br)
import Html.Attributes exposing (style, type_, disabled, checked)
import Html.Events exposing (onCheck)

type Msg
  = SetShouldStore Bool
  | SetStorage Storage

view :  Bool -> Bool -> Storage -> Html Msg
view canStore shouldStore storage =
  span
    []
    [ label
        [ style
            [ ( "color"
              , if not canStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not canStore)
            , checked shouldStore
            , onCheck SetShouldStore
            ]
            []
        , text " Remember my settings"
        ]
    , br [] []
    , label
        [ style
            [ ( "color"
              , if not shouldStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not shouldStore)
            , checked (shouldStore && storage.startEmpty)
            , onCheck (\x -> SetStorage { storage | startEmpty = x })
            ]
            []
        , text " Start with an empty textbox"
        ]
    , br [] []
    , label
        [ style
            [ ( "color"
              , if not shouldStore then
                  "GrayText"
                else
                  ""
              )
            ]
        ]
        [ input
            [ type_ "checkbox"
            , disabled (not shouldStore)
            , checked (shouldStore && storage.unsavedWarning)
            , onCheck (\x -> SetStorage { storage | unsavedWarning = x })
            ]
            []
        , text " Warn me when I close without saving"
        ]
    ]
