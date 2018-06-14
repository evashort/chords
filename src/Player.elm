module Player exposing
  ( Player, init, setTime, status, willChange, sequence, sequenceFinished
  , stop, pad, strum, arp
  )

import Arp
import AudioChange exposing (AudioChange(..))
import Chord exposing (Chord)
import Cliff exposing (Cliff)
import PlayStatus exposing (PlayStatus, IdChord)
import Note

type alias Player =
  { cliff : Cliff Bool
  , schedule : List Segment
  , pastLength : Int
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
  , pastLength = 0
  }

setTime : Float -> Player -> Maybe (Player, List Chord)
setTime now player =
  let
    pastLength =
      List.length (dropAfter now player.schedule)
  in
    if
      pastLength >= List.length player.schedule &&
        Cliff.nextHold now player.cliff == Nothing
    then
      Just
        ( init
        , List.reverse (List.map .chord player.schedule)
        )
    else if pastLength == player.pastLength then
      Nothing
    else
      Just
        ( { player | pastLength = pastLength }
        , []
        )

dropAfter : Float -> List Segment -> List Segment
dropAfter time schedule =
  case schedule of
    [] ->
      schedule
    segment :: rest ->
      if segment.stop > time then
        dropAfter time rest
      else
        schedule

status : Player -> PlayStatus
status player =
  let
    futureLength =
      List.length player.schedule - player.pastLength - 1
  in
    if futureLength < 0 then
      { active = -1
      , next = -1
      , stoppable = False
      }
    else if futureLength == 0 then
      case player.schedule of
        [] ->
          Debug.crash
            ("Player.status: Negative pastLength: " ++ toString player)
        current :: _ ->
          { active = current.id
          , next = -1
          , stoppable = current.stop == infinity
          }
    else
      case List.drop (futureLength - 1) player.schedule of
        future :: current :: _ ->
          { active = current.id
          , next = future.id
          , stoppable = False
          }
        _ ->
          Debug.crash
            ("Player.status: Negative pastLength: " ++ toString player)

willChange : Player -> Bool
willChange player =
  if List.length player.schedule == player.pastLength + 1 then
    case player.schedule of
      [] ->
        Debug.crash
          ("Player.willChange: Negative pastLength: " ++ toString player)
      current :: _ ->
        current.stop < infinity
  else
    player.schedule /= []

sequence : Player -> List Chord
sequence player =
  let
    futureLength =
      List.length player.schedule - player.pastLength - 1
  in
    List.reverse
      ( List.map
          .chord
          (List.drop futureLength player.schedule)
      )

sequenceFinished : Player -> Bool
sequenceFinished player =
  player.pastLength >= List.length player.schedule

stop : Float -> Player -> (Player, List Chord, List AudioChange)
stop now player =
  ( init
  , sequence
      { player
      | pastLength =
          List.length (dropAfter now player.schedule)
      }
  , [ MuteAllNotes { t = now, before = False } ]
  )

pad : Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
pad lowestNote { id, chord } now player =
  let
    truncatedSchedule =
      truncateAfter now player.schedule
  in
    ( { player
      | cliff = Cliff.dropAfter now player.cliff
      , schedule =
          addSegment
            (Segment id chord infinity)
            truncatedSchedule
      , pastLength = List.length truncatedSchedule
      }
    , List.concat
        [ stopOldChord now chord now truncatedSchedule
        , [ SetAttack 0.2
          , SetPeak 0.25
          , SetDecay infinity
          ]
        , List.map
            (AddNote << Note.mapTime (always now))
            (Arp.strum lowestNote chord)
        ]
    )

strum :
  Float -> Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
strum strumInterval lowestNote { id, chord } now player =
  let
    truncatedSchedule =
      truncateAfter now player.schedule
  in
    ( { player
      | cliff = Cliff.dropAfter now player.cliff
      , schedule =
          addSegment
            (Segment id chord (now + 2.25))
            truncatedSchedule
      , pastLength = List.length truncatedSchedule
      }
    , List.concat
        [ stopOldChord now chord now truncatedSchedule
        , [ SetAttack 0
          , SetPeak 0.5
          , SetDecay 3
          ]
        , List.map
            ( AddNote <<
                Note.mapTime ((+) now << (*) strumInterval)
            )
            (Arp.strum lowestNote chord)
        ]
    )

arp :
  Float -> Int -> IdChord -> Float -> Player -> (Player, List AudioChange)
arp beatInterval lowestNote { id, chord } now player =
  let
    ( startTime, highStart ) =
      Maybe.withDefault
        ( now, False )
        (Cliff.nextHold now player.cliff)
  in let
    truncatedSchedule =
      truncateAfter startTime player.schedule
  in let
    schedule =
      addSegment
        (Segment id chord (startTime + 4 * beatInterval))
        truncatedSchedule
  in
    ( { cliff =
          Cliff.addHolds
            startTime
            (2 * beatInterval)
            [ True, False ]
            player.cliff
      , schedule = schedule
      , pastLength = List.length (dropAfter now schedule)
      }
    , List.concat
        [ stopOldChord startTime chord now truncatedSchedule
        , [ SetAttack 0
          , SetPeak 0.5
          , SetDecay 1.5
          ]
        , List.map
            ( AddNote <<
                Note.mapTime
                  (max now << (+) startTime << (*) beatInterval)
            )
            ( if highStart then
                Arp.continuation lowestNote chord
              else
                Arp.intro lowestNote chord
            )
        ]
    )

truncateAfter : Float -> List Segment -> List Segment
truncateAfter time schedule =
  case schedule of
    [] ->
      schedule
    [ segment ] ->
      if segment.stop > time then
        [ { segment | stop = time } ]
      else
        schedule
    segment :: previous :: rest ->
      if previous.stop < time then
        if segment.stop > time then
          { segment | stop = time } :: previous :: rest
        else
          schedule
      else
        truncateAfter time (previous :: rest)

addSegment : Segment -> List Segment -> List Segment
addSegment segment schedule =
  case schedule of
    [] ->
      [ segment ]
    current :: rest ->
      if current.id == segment.id then
        segment :: rest
      else
        segment :: schedule

stopOldChord : Float -> Chord -> Float -> List Segment -> List AudioChange
stopOldChord startTime chord now schedule =
  let
    changeTime =
      { t = max now startTime
      , before = now < startTime
      }
  in
    case schedule of
      [] ->
        [ CancelFutureNotes changeTime ]
      segment :: _ ->
        if segment.stop == infinity then
          [ SetDecay 0.5, CancelFutureNotes changeTime ]
        else if segment.chord /= chord then
          [ MuteAllNotes changeTime ]
        else
          [ CancelFutureNotes changeTime ]

infinity : Float
infinity = 1/0
