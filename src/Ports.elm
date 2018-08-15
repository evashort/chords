port module Ports exposing
  ( changeAudio, playing, stopAudio, setVolume
  , initMeter
  , initTheater, focusTheater, replace, undoAndReplace, hardUndo
  , text
  , initHarp, harpPlucked, Pluck
  , initStorage
  , setTitle
  , scrollIntoView, escape
  )

import Json.Encode as Encode

port changeAudio : List Encode.Value -> Cmd msg
port playing : (Bool -> msg) -> Sub msg
port stopAudio : () -> Cmd msg
port setVolume : Float -> Cmd msg

port initMeter : () -> Cmd msg

port initTheater : Encode.Value -> Cmd msg
port focusTheater : () -> Cmd msg
port replace : Encode.Value -> Cmd msg
port undoAndReplace : Encode.Value -> Cmd msg
port hardUndo : () -> Cmd msg

port text : (String -> msg) -> Sub msg

port initHarp : () -> Cmd msg
port harpPlucked : (Pluck -> msg) -> Sub msg

port initStorage : () -> Cmd msg

port setTitle : String -> Cmd msg

type alias Pluck =
  { now : Float
  , mutes : List Int
  , pitches : List Int
  }

port scrollIntoView : String -> Cmd msg
port escape : (() -> msg) -> Sub msg
