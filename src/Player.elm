module Player exposing
  ( Player, init, setTime, status, willChange, sequence, sequenceFinished
  , lastPlayed, stop, pad, strum, arp, strumPattern
  )

import Arp
import AudioChange exposing (AudioChange(..))
import Chord exposing (Chord)
import Cliff exposing (Hold, Region, Cliff)
import IdChord exposing (IdChord)
import Note exposing (Note)
import PlayStatus exposing (PlayStatus)
import StrumPattern exposing (StrumPattern, StrumNote)

type alias Player =
  { cliff : Cliff Bool
  , schedule : List Segment
  , unfinishedCount : Int
  }

type alias Segment =
  { id : Int
  , chord : Chord
  , stop : Float
  }

init : Player
init =
  { cliff = []
  , schedule = []
  , unfinishedCount = 0
  }

setTime : Float -> Player -> Maybe Player
setTime now player =
  let
    unfinishedCount = countUnfinished now player.schedule
  in
    if unfinishedCount == player.unfinishedCount then
      Nothing
    else
      Just { player | unfinishedCount = unfinishedCount }

status : Bool -> Player -> PlayStatus
status showSilent player =
  if player.unfinishedCount <= 0 then
    if showSilent then
      case player.schedule of
        [] ->
          PlayStatus.Cleared
        current :: _ ->
          PlayStatus.Selected current.id
    else
      PlayStatus.Cleared
  else if player.unfinishedCount == 1 then
    case player.schedule of
      [] ->
        Debug.todo
          ( "Player.status: Inconsistent state: " ++
              Debug.toString player
          )
      current :: _ ->
        PlayStatus.Playing
          { active = current.id
          , stopButton = current.stop == infinity
          , next = Nothing
          }
  else
    case List.drop (player.unfinishedCount - 2) player.schedule of
      future :: current :: _ ->
        PlayStatus.Playing
          { active = current.id
          , stopButton = False
          , next = Just future.id
          }
      _ ->
        Debug.todo
          ( "Player.status: Inconsistent state: " ++
              Debug.toString player
          )

willChange : Player -> Bool
willChange player =
  if player.unfinishedCount == 1 then
    case player.schedule of
      [] ->
        Debug.todo
          ( "Player.willChange: Inconsistent state: " ++
              Debug.toString player
          )
      current :: _ ->
        current.stop < infinity
  else
    player.unfinishedCount > 0

sequence : Player -> List Chord
sequence player =
  let
    startedSegments =
      List.drop (player.unfinishedCount - 1) player.schedule
  in
    removeDuplicates
      (List.reverse (List.map .chord startedSegments))

removeDuplicates : List a -> List a
removeDuplicates xs =
  case xs of
    x :: y :: rest ->
      if x == y then
        removeDuplicates (y :: rest)
      else
        x :: removeDuplicates (y :: rest)
    other ->
      other

sequenceFinished : Player -> Bool
sequenceFinished player =
  player.unfinishedCount <= 0

lastPlayed : Player -> Maybe IdChord
lastPlayed player =
  case
    List.drop (player.unfinishedCount - 1) player.schedule
  of
    segment :: _ ->
      Just (IdChord segment.id segment.chord)
    [] ->
      Nothing

stop : Float -> Player -> Player
stop now player =
  if player.unfinishedCount > 0 then
    { cliff = []
    , schedule =
        stopScheduleAt now player.schedule
    , unfinishedCount = 0
    }
  else
    player

pad :
  Int -> IdChord -> Float -> Player -> (Player, List Chord, List AudioChange)
pad lowestPitch { id, chord } now player =
  let
    shouldInit = countUnfinished now player.schedule <= 0
  in
    ( addSegment
        now
        (Region now 0 [])
        (Segment id chord infinity)
        (if shouldInit then init else player)
    , if shouldInit then
        sequence { player | unfinishedCount = 0 }
      else
        []
    , (::)
        (Mute now)
        ( List.map
            (AddPadNote << Note.mapTime ((+) now << (*) 0.009))
            (Arp.pad lowestPitch chord)
        )
    )

strum :
  Float -> Int -> IdChord -> Float -> Player ->
    (Player, List Chord, List AudioChange)
strum strumInterval lowestPitch { id, chord } now player =
  let
    shouldInit = countUnfinished now player.schedule <= 0
  in
    ( addSegment
        now
        (Region now 0 [])
        (Segment id chord (now + 2.25))
        (if shouldInit then init else player)
    , if shouldInit then
        sequence { player | unfinishedCount = 0 }
      else
        []
    , (++)
        [ Mute now
        , AddAlarm (now + 2.25)
        ]
        ( List.map
            ( AddGuitarNote <<
                Note.mapTime ((+) now << (*) strumInterval)
            )
            (Arp.strum lowestPitch chord)
        )
    )

arp :
  Float -> Int -> IdChord -> Float -> Player ->
    (Player, List Chord, List AudioChange)
arp beatInterval lowestPitch { id, chord } now player =
  let
    nextHold = Cliff.nextHold now player.cliff
  in
  let
    shouldInit =
      nextHold == Nothing &&
        countUnfinished now player.schedule <= 0
    ( startTime, highStart ) =
      Maybe.withDefault ( now, False ) nextHold
  in
    ( addSegment
        now
        ( Region
            startTime
            (2 * beatInterval)
            [ Hold 1 True, Hold 2 False ]
        )
        (Segment id chord (startTime + 4 * beatInterval))
        (if shouldInit then init else player)
    , if shouldInit then
        sequence { player | unfinishedCount = 0 }
      else
        []
    , (++)
        [ Mute startTime
        , AddAlarm startTime
        , AddAlarm (startTime + 4 * beatInterval)
        ]
        ( List.map
            ( AddPianoNote <<
                Note.mapTime
                  ((+) startTime << (*) beatInterval)
            )
            ( if highStart then
                Arp.continuation lowestPitch chord
              else
                Arp.intro lowestPitch chord
            )
        )
    )

strumPattern :
  StrumPattern -> Float -> Int -> IdChord -> Float -> Player ->
    (Player, List Chord, List AudioChange)
strumPattern pattern beatInterval lowestPitch { id, chord } now player =
  let
    nextHold = Cliff.nextHold now player.cliff
  in
  let
    shouldInit =
      nextHold == Nothing &&
        countUnfinished now player.schedule <= 0
    ( startTime, highStart ) =
      Maybe.withDefault ( now, False ) nextHold
  in
  let
    beatCount =
      case pattern of
        StrumPattern.Basic ->
          8
        StrumPattern.Indie ->
          4
        StrumPattern.Modern ->
          if highStart then 6 else 4
    holds =
      case pattern of
        StrumPattern.Basic ->
          [ Hold 2 False, Hold 4 False ]
        StrumPattern.Indie ->
          [ Hold 1 False, Hold 2 False ]
        StrumPattern.Modern ->
          if highStart then
            [ Hold 1 False, Hold 2 True, Hold 3 False ]
          else
            [ Hold 1 True, Hold 2 False ]
  in
    ( addSegment
        now
        (Region startTime (2 * beatInterval) holds)
        ( Segment
            id
            chord
            (startTime + beatCount * beatInterval)
        )
        (if shouldInit then init else player)
    , if shouldInit then
        sequence { player | unfinishedCount = 0 }
      else
        []
    , (++)
        [ Mute startTime
        , AddAlarm startTime
        , AddAlarm (startTime + beatCount * beatInterval)
        ]
        ( List.map
            ( AddGuitarNote <<
                processStrumNote startTime beatInterval
            )
            (StrumPattern.notes pattern highStart lowestPitch chord)
        )
    )

processStrumNote : Float -> Float -> StrumNote -> Note
processStrumNote startTime beatInterval strumNote =
  { v = strumNote.v
  , t =
      startTime +
        beatInterval * strumNote.t +
        0.009 * toFloat strumNote.strumIndex
  , f = strumNote.f
  }

addSegment : Float -> Region Bool -> Segment -> Player -> Player
addSegment now region segment player =
  let
    newSchedule =
      segment ::
        stopScheduleAt region.origin player.schedule
  in
    { cliff = Cliff.addRegion region player.cliff
    , schedule = newSchedule
    , unfinishedCount = countUnfinished now newSchedule
    }

stopScheduleAt : Float -> List Segment -> List Segment
stopScheduleAt t schedule =
  case schedule of
    [] ->
      schedule
    [ segment ] ->
      if segment.stop > t then
        [ { segment | stop = t } ]
      else
        schedule
    segment :: previous :: rest ->
      if previous.stop < t then
        if segment.stop > t then
          { segment | stop = t } :: previous :: rest
        else
          schedule
      else
        stopScheduleAt t (previous :: rest)

countUnfinished : Float -> List Segment -> Int
countUnfinished now schedule =
  case schedule of
    [] ->
      0
    segment :: rest ->
      if segment.stop > now then
        1 + countUnfinished now rest
      else
        0

infinity : Float
infinity = 1/0
