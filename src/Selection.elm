module Selection exposing
  ( Selection(..), setTime, stop
  , member, scheduled, playing, willChange
  , sequence, sequenceAtTime
  )

import Chord exposing (Chord)
import Click exposing (Click)
import IdChord exposing (IdChord)
import Player exposing (Player)
import PlayStyle exposing (PlayStyle)
import Sound

type Selection
  = Static (Maybe IdChord)
  | Dynamic Player
  | Custom

setTime : Float -> Selection -> Selection
setTime now selection =
  case selection of
    Dynamic player ->
      case Player.setTime now player of
        Nothing ->
          selection
        Just newPlayer ->
          Dynamic newPlayer
    _ ->
      selection

stop : Float -> Selection -> Selection
stop now selection =
  case selection of
    Dynamic player ->
      Dynamic (Player.stop now player)
    _ ->
      selection

member : Selection -> Int -> Bool
member selection id =
  case selection of
    Static (Just idChord) ->
      idChord.id == id
    Dynamic player ->
      (Player.currentIdChord player).id == id
    _ ->
      False

scheduled : Selection -> Int -> Bool
scheduled selection id =
  case selection of
    Dynamic player ->
      Player.scheduled player id
    _ ->
      False

playing : Selection -> Bool
playing selection =
  case selection of
    Dynamic player ->
      Player.playing player
    _ ->
      False

willChange : Selection -> Bool
willChange selection =
  case selection of
    Dynamic player ->
      Player.willChange player
    _ ->
      False

sequence : Selection -> List Chord
sequence selection =
  case selection of
    Dynamic player ->
      Player.sequence player
    _ ->
      []

sequenceAtTime : Float -> Selection -> List Chord
sequenceAtTime now selection =
  case selection of
    Dynamic player ->
      Player.sequenceAtTime now player
    _ ->
      []
