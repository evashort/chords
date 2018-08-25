port module Ports exposing
  ( changeAudio, playing, setVolume, currentTime
  , initMeter
  , initTheater, focusTheater, replace, undoAndReplace, hardUndo
  , text
  , initHarp, clearPlucks, harpPlucked, Pluck
  , initStorage
  , setTitle
  , download, File
  , scrollIntoView, escape
  )

import Json.Encode as Encode

port changeAudio : List Encode.Value -> Cmd msg
port playing : (Bool -> msg) -> Sub msg
port stopAudio : () -> Cmd msg
port setVolume : Float -> Cmd msg
port currentTime : (Float -> msg) -> Sub msg

port initMeter : () -> Cmd msg

port initTheater : Encode.Value -> Cmd msg
port focusTheater : () -> Cmd msg
port replace : Encode.Value -> Cmd msg
port undoAndReplace : Encode.Value -> Cmd msg
port hardUndo : () -> Cmd msg

port text : (String -> msg) -> Sub msg

port initHarp : () -> Cmd msg
port clearPlucks : () -> Cmd msg
port harpPlucked : (Pluck -> msg) -> Sub msg
type alias Pluck =
  { now : Float
  , mutes : List Int
  , pitches : List Int
  }

port initStorage : () -> Cmd msg

port setTitle : String -> Cmd msg

port download : File -> Cmd msg
type alias File =
  { name : String
  , base16 : String
  }

port scrollIntoView : String -> Cmd msg
port escape : (() -> msg) -> Sub msg
