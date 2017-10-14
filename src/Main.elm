port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { audioMsgs : List AudioMsg
  }

init : ( Model, Cmd Msg )
init =
  ( { audioMsgs = []
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = NeedsTime AudioMsg
  | CurrentTime Float

type AudioMsg = PlayChord

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime audioMsg ->
      ( { model | audioMsgs = audioMsg :: model.audioMsgs }, timeRequest () )
    
    CurrentTime t ->
      let
        ( newModel, cmds ) =
          List.foldr
            (batchUpdate <| timeUpdate t)
            ( { model | audioMsgs = [] }, [] )
            model.audioMsgs
      in
        ( newModel, Cmd.batch <| List.reverse cmds )

timeUpdate : Float -> AudioMsg -> Model -> ( Model, Cmd Msg )
timeUpdate t msg model =
  case msg of
    PlayChord ->
      ( model, playNote { t = t, f = 440 } )

batchUpdate : (m -> Model -> ( Model, c )) -> m -> ( Model, List c ) -> ( Model, List c )
batchUpdate update msg ( model, cmds ) =
  let ( newModel, cmd ) = update msg model in
    ( newModel, cmd :: cmds )

-- SUBSCRIPTIONS

port timeRequest : () -> Cmd msg
port currentTime : (Float -> msg) -> Sub msg

type alias Note =
  { t : Float
  , f : Float
  }
port playNote : Note -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ currentTime CurrentTime
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick <| NeedsTime PlayChord ] [ text "do it" ]
    ]
