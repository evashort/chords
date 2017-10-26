port module Main exposing (..)

import AudioTime
import Chord exposing (Chord)
import Highlight exposing (Highlight)
import Metronome exposing (Metronome)
import Tokenizer

import AnimationFrame
import Html exposing (Html, a, div, pre, span, text, textarea)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onMouseDown, on, targetValue)
import Json.Decode
import Json.Encode
import Task exposing (Task)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { playing : Maybe PlayInfo
  , text : String
  }

type alias PlayInfo =
  { metronome : Metronome
  , chord : Chord
  , nextChord : Maybe ScheduledChord
  }

type alias ScheduledChord =
  { chord : Chord
  , tick : Int
  }

init : ( Model, Cmd Msg )
init =
  ( { playing = Nothing
    , text = "C G Am F"
    }
  , Cmd.none
  )

chords : List Chord
chords =
  [ [ 48, 52, 55 ]
  , [ 50, 53, 57 ]
  , [ 52, 55, 59 ]
  , [ 53, 57, 60 ]
  , [ 55, 59, 62 ]
  , [ 57, 60, 64 ]
  , [ 59, 62, 65 ]
  ]

-- UPDATE

type Msg
  = NeedsTime (Float -> Msg)
  | CurrentTime Float
  | PlayChord ( Chord, Float )
  | TextEdited String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NeedsTime partialMsg ->
      ( model, Task.perform partialMsg AudioTime.now )

    CurrentTime now ->
      case model.playing of
        Just p ->
          if now >= Metronome.getStop p.metronome then
            ( { model | playing = Nothing }, Cmd.none )
          else
            case p.nextChord of
              Nothing -> ( model, Cmd.none )
              Just { chord, tick } ->
                if now >= Metronome.getTickTime p.metronome tick then
                  ( { model
                    | playing =
                        Just { p | chord = chord, nextChord = Nothing }
                    }
                  , Cmd.none
                  )
                else
                  ( model, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    PlayChord ( chord, now ) ->
      let
        start =
          case model.playing of
            Nothing -> now
            Just p -> p.metronome.start
      in let
        oldTicks =
          case model.playing of
            Nothing -> 0
            Just p -> p.metronome.ticks
      in let
        changeTick =
          case model.playing of
            Nothing -> 0
            Just p -> Metronome.getNextBeat p.metronome now
      in let
        arpeggio = Chord.arpeggio chord
      in let
        arpeggioStartIndex = changeTick % List.length arpeggio
      in let
        missingTicks =
          max 0 (minTicks - (List.length arpeggio - arpeggioStartIndex))
      in let
        extraCopies =
          (missingTicks + List.length arpeggio - 1) // List.length arpeggio
      in let
        editedArpeggio =
          List.concat <|
            (List.drop arpeggioStartIndex arpeggio) ::
              List.repeat extraCopies arpeggio
      in let
        m = { start = start, ticks = changeTick + List.length editedArpeggio }
      in let
        changeTime = Metronome.getTickTime m changeTick
      in let
        oldChord =
          case model.playing of
            Nothing -> Nothing
            Just p ->
              if changeTime > now then Just p.chord else Nothing
      in let
        p =
          case oldChord of
            Nothing ->
              { metronome = m
              , chord = chord
              , nextChord = Nothing
              }
            Just c ->
              { metronome = m
              , chord = c
              , nextChord = Just { chord = chord, tick = changeTick }
              }
      in let
        audioChanges =
          ( if changeTick < oldTicks then
              if now + latency >= changeTime then
                let notBeforeNote = max now changeTime in
                  [ CancelFutureNotes
                      { t = notBeforeNote, before = False }
                  , MuteLoudestNote notBeforeNote
                  ]
              else
                [ CancelFutureNotes { t = changeTime, before = True } ]
            else
              []
          ) ++
            (midiToNotes start changeTick now editedArpeggio)
      in
        ( { model | playing = Just p }
        , changeAudioUsingJson (List.map audioChangeToJson audioChanges)
        )

    TextEdited newText ->
      ( { model | text = newText }, Cmd.none )

minTicks : Int
minTicks = 9

latency : Float
latency = 0.01

mtof : Int -> Float
mtof m =
  440 * 2 ^ (toFloat (m - 69) / 12)

-- root octave is midi notes 48 - 59 (C2 - B2)

midiToNotes : Float -> Int -> Float -> List Int -> List AudioChange
midiToNotes start changeTick now arpeggio =
  List.map2
    (toNote now)
    ( List.map
        ((+) start << (*) Metronome.interval << toFloat)
        (List.range changeTick <| changeTick + List.length arpeggio)
    )
    (List.map mtof arpeggio)

toNote : Float -> Float -> Float -> AudioChange
toNote now t f =
  NewNote (Note (max now t) f)

-- SUBSCRIPTIONS

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
    MuteLoudestNote t ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "muteLoudest" )
        , ( "t", Json.Encode.float t )
        ]
    MuteAllNotes ct ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "mute" )
        , ( "t", Json.Encode.float ct.t )
        , ( "before", Json.Encode.bool ct.before )
        ]
    CancelFutureNotes ct ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "cancel" )
        , ( "t", Json.Encode.float ct.t )
        , ( "before", Json.Encode.bool ct.before )
        ]

type AudioChange
  = NewNote Note
  | MuteLoudestNote Float
  | MuteAllNotes ChangeTime
  | CancelFutureNotes ChangeTime

type alias Note =
  { t : Float
  , f : Float
  }

type alias ChangeTime =
  { t : Float
  , before : Bool
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.playing of
    Nothing -> Sub.none
    Just _ -> AnimationFrame.times (always (NeedsTime CurrentTime))

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ style
        [ ( "font-family", "calibri, helvetica, arial, sans-serif" )
        ]
    ]
    [ div
        [ style
            [ ( "width", "500px" )
            , ( "position", "relative" )
            , ( "border-style", "inset" )
            , ( "border-width", "2px" )
            , ( "border-color", "#e3e3e3")
            , ( "font-size", "20pt" )
            ]
        ]
        [ textarea
            [ on "input" (Json.Decode.map TextEdited targetValue)
            , style
                [ ( "font", "inherit" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "padding", "10px" )
                , ( "border", "none" )
                , ( "margin", "0px" )
                , ( "position", "absolute" )
                , ( "resize", "none" )
                , ( "overflow", "hidden" )
                , ( "box-sizing", "border-box" )
                , ( "background", "transparent" )
                ]
            ]
            [ text model.text
            ]
        , pre
            [ style
                [ ( "font", "inherit")
                , ( "padding", "10px" )
                , ( "margin", "0px" )
                , ( "white-space", "pre-wrap" )
                , ( "word-wrap", "break-word" )
                , ( "color", "transparent" )
                ]
            ]
            ( List.map Highlight.view <|
                Highlight.group <|
                  List.map Highlight.fromText <|
                    Tokenizer.tokenize model.text ++ [ "\n" ]
            )
        ]
    , div
        [ style [ ( "height", "200px" ) ] ] <|
        List.map
          ( viewChord
            ( case model.playing of
                Nothing -> []
                Just p -> p.chord
            )
            ( case Maybe.andThen .nextChord model.playing of
                Nothing -> []
                Just { chord, tick } -> chord
            )
          )
          chords
    , div []
        [ a
            [ href "https://github.com/evanshort73/chords" ]
            [ text "GitHub" ]
        ]
    ]

viewChord : Chord -> Chord -> Chord -> Html Msg
viewChord activeChord nextChord chord =
  let
    selected =
      chord == activeChord || chord == nextChord
  in
    span
      [ style
          [ ( "border-style"
            , if chord == nextChord then
                "dashed"
              else
                "solid"
            )
          , ( "border-width", "5px" )
          , ( "display", "inline-block" )
          , ( "margin-right", "-5px" )
          , ( "border-color"
            , if selected then
                "#3399ff"
              else
                "transparent"
            )
          , ( "border-radius", "10px" )
          ]
      ]
      [ span
          [ onMouseDown <| NeedsTime (PlayChord << (,) chord)
          , style
              [ ( "background", Chord.bg chord )
              , ( "color", Chord.fg chord )
              , ( "width", "50px" )
              , ( "line-height", "50px" )
              , ( "font-size", "20pt" )
              , ( "text-align", "center" )
              , ( "display", "inline-block" )
              , ( "border-radius", "5px" )
              , ( "box-shadow", "1px 1px 3px rgba(0, 0, 0, 0.6)" )
              , ( "cursor", "pointer" )
              ]
          ]
          [ text (Chord.prettyName chord) ]
      ]
