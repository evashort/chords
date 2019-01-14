module Selection exposing
  ( Selection(..), member, scheduled, setTime, sequence, sequenceFinished
  , sequenceAtTime, willChange, canContinue, stop
  )

import Chord exposing (Chord)
import Cliff
import IdChord exposing (IdChord)
import Player exposing (Player)
import PlayStyle exposing (PlayStyle)

type Selection
  = Static (Maybe IdChord)
  | Dynamic Player
  | Custom

member : Int -> Selection -> Bool
member id selection =
  case selection of
    Static (Just idChord) ->
      idChord.id == id
    Dynamic player ->
      case List.drop (player.unfinishedCount - 1) player.schedule of
        current :: _ ->
          id == current.id
        _ ->
          False
    _ ->
      False

scheduled : Int -> Selection -> Bool
scheduled id selection =
  case selection of
    Dynamic player ->
      List.member
        id
        ( List.map
            .id
            (List.take player.unfinishedCount player.schedule)
        )
    _ ->
      False

infinity : Float
infinity = 1/0

setTime : Float -> Selection -> Maybe Selection
setTime now selection =
  case selection of
    Dynamic player ->
      Maybe.map Dynamic (Player.setTime now player)
    _ ->
      Nothing

sequence : Selection -> List Chord
sequence selection =
  case selection of
    Dynamic player ->
      Player.sequence player
    _ ->
      []

sequenceFinished : Selection -> Bool
sequenceFinished selection =
  case selection of
    Dynamic player ->
      player.unfinishedCount <= 0
    _ ->
      True

sequenceAtTime : Float -> Selection -> List Chord
sequenceAtTime now selection =
  case selection of
    Dynamic player ->
      case Player.setTime now player of
        Just newPlayer ->
          Player.sequence newPlayer
        Nothing ->
          Player.sequence player
    _ ->
      []

willChange : Selection -> Bool
willChange selection =
  case selection of
    Dynamic player ->
      if player.unfinishedCount == 1 then
        case player.schedule of
          [] ->
            Debug.todo
              ( "Selection.willChange: Inconsistent player state: " ++
                  Debug.toString player
              )
          current :: _ ->
            current.stop < infinity
      else
        player.unfinishedCount > 0
    _ ->
      False

chordClicked : IdChord -> Float -> PlayStyle -> Selection -> Selection
chordClicked idChord now playStyle selection =
  case selection of
    Dynamic player ->
      if player.unfinishedCount > 0 then
        Just player
      else if playStyle == PlayStyle.Pad then
        False
      else
        Cliff.nextHold now player.cliff /= Nothing
    _ ->
      False

stop : Float -> Selection -> Selection
stop now selection =
  case selection of
    Dynamic player ->
      if player.unfinishedCount > 0 then
        Dynamic (Player.stop now player)
      else
        selection
    _ ->
      selection
