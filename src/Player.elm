module Player exposing
  ( Player, init, setTime, status, willChange, sequence, sequenceFinished
  , stop, pad, strum, arp, strumPattern
  )

import Arp
import AudioChange exposing (AudioChange(..))
import Chord exposing (Chord)
import Cliff exposing (Hold, Region, Cliff)
import IdChord exposing (IdChord, PlayStatus)
import Note exposing (Note)
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

setTime : Float -> Player -> Maybe (Player, List Chord)
setTime now player =
  let
    unfinishedCount = countUnfinished now player.schedule
  in
    if
      unfinishedCount <= 0 &&
        Cliff.nextHold now player.cliff == Nothing
    then
      Just
        ( init
        , sequence { player | unfinishedCount = 0 }
        )
    else if unfinishedCount == player.unfinishedCount then
      Nothing
    else
      Just
        ( { player | unfinishedCount = unfinishedCount }
        , []
        )

status : Player -> PlayStatus
status player =
  if player.unfinishedCount <= 0 then
    { active = -1
    , next = -1
    , stoppable = False
    }
  else if player.unfinishedCount == 1 then
    case player.schedule of
      [] ->
        Debug.crash
          ("Player.status: Inconsistent state: " ++ toString player)
      current :: _ ->
        { active = current.id
        , next = -1
        , stoppable = current.stop == infinity
        }
  else
    case List.drop (player.unfinishedCount - 2) player.schedule of
      future :: current :: _ ->
        { active = current.id
        , next = future.id
        , stoppable = False
        }
      _ ->
        Debug.crash
          ("Player.status: Inconsistent state: " ++ toString player)

willChange : Player -> Bool
willChange player =
  if player.unfinishedCount == 1 then
    case player.schedule of
      [] ->
        Debug.crash
          ("Player.willChange: Inconsistent state: " ++ toString player)
      current :: _ ->
        current.stop < infinity
  else
    player.schedule /= []

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

stop : Float -> Player -> (Player, List Chord, List AudioChange)
stop now player =
  ( init
  , sequence
      { player
      | unfinishedCount =
          countUnfinished now player.schedule
      }
  , [ Mute now ]
  )

pad : Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
pad lowestPitch { id, chord } now player =
  ( addSegment
      now
      (Region now 0 [])
      (Segment id chord infinity)
      player
  , (::)
      (Mute now)
      ( List.map
          (AddPadNote << Note.mapTime (always now))
          (Arp.pad lowestPitch chord)
      )
  )

strum :
  Float -> Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
strum strumInterval lowestPitch { id, chord } now player =
  ( addSegment
      now
      (Region now 0 [])
      (Segment id chord (now + 2.25))
      player
  , (::)
      (Mute now)
      ( List.map
          ( AddGuitarNote <<
              Note.mapTime ((+) now << (*) strumInterval)
          )
          (Arp.strum lowestPitch chord)
      )
  )

arp :
  Float -> Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
arp beatInterval lowestPitch { id, chord } now player =
  let
    ( startTime, highStart ) =
      Maybe.withDefault
        ( now, False )
        (Cliff.nextHold now player.cliff)
  in
    ( addSegment
        now
        ( Region
            startTime
            (2 * beatInterval)
            [ Hold 1 True, Hold 2 False ]
        )
        (Segment id chord (startTime + 4 * beatInterval))
        player
    , (::)
        (Mute startTime)
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
    (Player, List AudioChange)
strumPattern pattern beatInterval lowestPitch { id, chord } now player =
  let
    ( startTime, highStart ) =
      Maybe.withDefault
        ( now, False )
        (Cliff.nextHold now player.cliff)
  in let
    beatCount =
      case pattern of
        StrumPattern.Basic ->
          8
        StrumPattern.Indie ->
          4
        StrumPattern.Modern ->
          if highStart then 6 else 4
  in let
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
        player
    , (::)
        (Mute startTime)
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
stopScheduleAt stop schedule =
  case schedule of
    [] ->
      schedule
    [ segment ] ->
      if segment.stop > stop then
        [ { segment | stop = stop } ]
      else
        schedule
    segment :: previous :: rest ->
      if previous.stop < stop then
        if segment.stop > stop then
          { segment | stop = stop } :: previous :: rest
        else
          schedule
      else
        stopScheduleAt stop (previous :: rest)

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
