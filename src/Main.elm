port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Encode

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
        ( newModel, changes ) =
          List.foldr
            (batchUpdate <| audioUpdate t)
            ( { model | audioMsgs = [] }, [ [] ] )
            model.audioMsgs
      in
        ( newModel, changeAudio <| List.concat <| List.reverse changes )

batchUpdate : (m -> Model -> ( Model, c )) -> m -> ( Model, List c ) -> ( Model, List c )
batchUpdate update msg ( model, cmds ) =
  let ( newModel, cmd ) = update msg model in
    ( newModel, cmd :: cmds )

audioUpdate : Float -> AudioMsg -> Model -> ( Model, List AudioChange )
audioUpdate t msg model =
  case msg of
    PlayChord ->
      ( model
      , MuteAllNotes t ::
          List.map
            NewNote
            (offsetsToNotes t 48 <| majorArpeggio ++ majorArpeggio)
      )

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

-- root octave is midi notes 48 - 59 (C2 - B2)

majorArpeggio : List Int
majorArpeggio = [ 12, 4, 7, 0, 4, 7, 0, 4 ]

offsetsToNotes : Float -> Int -> List Int -> List Note
offsetsToNotes t root offsets =
  List.map2
    Note
    ( List.map
        ((+) t << (*) 0.15 << toFloat)
        (List.range 0 <| List.length offsets)
    )
    (List.map (mtof << (+) root) offsets)

-- SUBSCRIPTIONS

port timeRequest : () -> Cmd msg
port currentTime : (Float -> msg) -> Sub msg

changeAudio : List AudioChange -> Cmd msg
changeAudio = changeAudioUsingJson << List.map audioChangeToJson

port changeAudioUsingJson : List Json.Encode.Value -> Cmd msg

audioChangeToJson : AudioChange -> Json.Encode.Value
audioChangeToJson change =
  case change of
    NewNote note ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "note" )
        , ( "t", Json.Encode.float note.t )
        , ( "f", Json.Encode.float note.f )
        ]
    MuteAllNotes t ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "mute" )
        , ( "t", Json.Encode.float t )
        ]

type AudioChange = NewNote Note | MuteAllNotes Float

type alias Note =
  { t : Float
  , f : Float
  }

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
