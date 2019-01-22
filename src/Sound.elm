module Sound exposing
  ( Sound, piano, guitar, pad, rest, up, down, noteOff, mute, cancel, alarm
  , mapTime, timeInRange, play
  )

import Note exposing (Note)
import Ports

import Json.Encode as Encode

type Sound
  = Piano Note
  | Guitar Note
  | Pad Note
  | Strum StrumNote
  | NoteOff Note
  | Mute Float
  | Cancel Float
  | Alarm Float

type alias Note =
  { time : Float
  , pitch : Int
  }

type alias StrumNote =
  { time : Float
  , offset : Float
  , pitch : Int
  , velocity : Float
  }

piano : Float -> Int -> Sound
piano time pitch =
  Piano (Note time pitch)

guitar : Float -> Int -> Sound
guitar time pitch =
  Guitar (Note time pitch)

pad : Float -> Int -> Sound
pad time pitch =
  Pad (Note time pitch)

rest : Float -> List Int -> List Sound
rest time pitches =
  []

up : Int -> Float -> Float -> List Int -> List Sound
up intVelocity missedStrings time pitches =
  down intVelocity missedStrings time (List.reverse pitches)

down : Int -> Float -> Float -> List Int -> List Sound
down intVelocity missedStrings time pitches =
  let
    velocity = 0.01 * toFloat intVelocity
    fullyMissedStrings = floor missedStrings
  in
    let
      noteCount = List.length pitches - fullyMissedStrings
      partialMiss = missedStrings - toFloat fullyMissedStrings
    in
      let
        velocities =
          List.append
            (List.repeat (noteCount - 1) velocity)
            [ (1 - partialMiss) * velocity ]
        offsets =
          List.map
            ((*) strumInterval << toFloat)
            (List.range 0 (noteCount - 1))
      in
        List.map3 (strum time) offsets pitches velocities

strum : Float -> Float -> Int -> Float -> Sound
strum time offset pitch velocity =
  Strum (StrumNote time offset pitch velocity)

strumInterval : Float
strumInterval = 0.009

noteOff : Float -> Int -> Sound
noteOff time pitch =
  NoteOff (Note time pitch)

mute : Float -> Sound
mute = Mute

cancel : Float -> Sound
cancel = Cancel

alarm : Float -> Sound
alarm = Alarm

mapTime : (Float -> Float) -> Sound -> Sound
mapTime f sound =
  case sound of
    Piano { time, pitch } ->
      Piano (Note (f time) pitch)
    Guitar { time, pitch } ->
      Guitar (Note (f time) pitch)
    Pad { time, pitch } ->
      Pad (Note (f time) pitch)
    Strum { time, offset, pitch, velocity } ->
      Strum (StrumNote (f time) offset pitch velocity)
    NoteOff { time, pitch } ->
      NoteOff (Note (f time) pitch)
    Mute time ->
      Mute (f time)
    Cancel time ->
      Cancel (f time)
    Alarm time ->
      Alarm (f time)

timeInRange : Float -> Float -> Sound -> Bool
timeInRange start stop sound =
  let time = getTime sound in
    start <= time && time < stop

getTime : Sound -> Float
getTime sound =
  case sound of
    Piano { time, pitch } -> time
    Guitar { time, pitch } -> time
    Pad { time, pitch } -> time
    Strum { time, offset, pitch, velocity } -> time
    NoteOff { time, pitch } -> time
    Mute time -> time
    Cancel time -> time
    Alarm time -> time

play : List Sound -> Cmd msg
play = Ports.sound << List.map toJson

toJson : Sound -> Encode.Value
toJson sound =
  case sound of
    Piano { time, pitch } ->
      Encode.object
        [ ( "type", Encode.string "piano" )
        , ( "t", Encode.float time )
        , ( "f", Encode.float (pitchFrequency pitch) )
        ]
    Guitar { time, pitch } ->
      Encode.object
        [ ( "type", Encode.string "guitar" )
        , ( "v", Encode.float 1 )
        , ( "t", Encode.float time )
        , ( "f", Encode.float (pitchFrequency pitch) )
        ]
    Pad { time, pitch } ->
      Encode.object
        [ ( "type", Encode.string "pad" )
        , ( "t", Encode.float time )
        , ( "f", Encode.float (pitchFrequency pitch) )
        ]
    Strum { time, offset, pitch, velocity } ->
      Encode.object
        [ ( "type", Encode.string "guitar" )
        , ( "v", Encode.float velocity )
        , ( "t", Encode.float (time + offset) )
        , ( "f", Encode.float (pitchFrequency pitch) )
        ]
    NoteOff { time, pitch } ->
      Encode.object
        [ ( "type", Encode.string "noteOff" )
        , ( "t", Encode.float time )
        , ( "f", Encode.float (pitchFrequency pitch) )
        ]
    Mute time ->
      Encode.object
        [ ( "type", Encode.string "mute" )
        , ( "t", Encode.float time )
        ]
    Cancel time ->
      Encode.object
        [ ( "type", Encode.string "cancel" )
        , ( "t", Encode.float time )
        ]
    Alarm time ->
      Encode.object
        [ ( "type", Encode.string "alarm" )
        , ( "t", Encode.float time )
        ]

pitchFrequency : Int -> Float
pitchFrequency pitch =
  440 * 2 ^ (toFloat (pitch - 69) / 12)
