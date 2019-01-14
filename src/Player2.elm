module Player2 exposing
  ( start, setBpm, setGenre, addClick, setTime, stop
  , member, scheduled, playing, willChange
  , play, sequence, sequenceAtTime
  )

import Click exposing (Click)
import Genre exposing (Genre)
import IdChord exposing (IdChord)
import Metro exposing (Metro)

type alias Player =
  { metro : Metro
  , genre : Genre
  , genreStart : Int
  , clicks : List Click
  , stopBeat : Float
  , unfinishedCount : Int
  , modifiedTime : Float
  }

start : Float -> Genre -> Click -> Player
start bpm genre click =
  { metro = Metro.setBpm bpm click.time []
  , genre = genre
  , genreStart = 0
  , clicks = [ { click | time = 0 } ]
  , stopBeat =
      if genre == Genre.Pad then
        infinity
      else
        4
  , unfinishedCount = 1
  , modifiedTime = click.time
  }

setBpm : Float -> Float -> Player -> Player
setBpm bpm now player =
  { player
  | metro = Metro.setBpm bpm now player.metro
  }

setGenre : Genre -> Float -> Player -> Player
setGenre genre now player =
  let genreStart = Metro.nextOpening player.metro now in
    { player
    | genre = genre
    , genreStart = genreStart
    , stopBeat =
        if genre == Genre.Pad then
          infinity
        else
          toFloat (genreStart + 4)
    , modifiedTime = now
    }

addClick : Click -> Player -> Maybe Player
addClick click player =
  let
    beat =
      if Genre.isQuantized player.genre then
        toFloat (Metro.nextOpening player.metro click.time)
      else
        Metro.getBeat player.metro click.time
  in
    if beat > player.stopBeat then
      Nothing
    else
      let
        newClicks =
          Click.add { click | time = beat } player.clicks
        newStopBeat =
          getStopBeat player.genre player.genreStart beat
      in
        Just
          { player
          | clicks = newClicks
          , stopBeat = newStopBeat
          , unfinishedCount =
              countUnfinished newClicks newStopBeat beat
          , modifiedTime =
              if Genre.isQuantized player.genre then
                click.time
              else
                player.modifiedTime
          }

getStopBeat : Genre -> Int -> Float -> Float
getStopBeat genre genreStart beat =
  if genre == Genre.Pad then
    infinity
  else if genre == Genre.Modern then
    toFloat
      (4 + ceilingWithInterval 4 (beat - toFloat genreStart))
  else
    toFloat (4 + ceilingWithInterval 2 beat)

infinity : Float
infinity = 1 / 0

ceilingWithInterval : Int -> Float -> Int
ceilingWithInterval interval x =
  interval * ceiling (x / interval)

countUnfinished : List Click -> Float -> Float -> Int
countUnfinished clicks stopBeat beat =
  if beat >= stopBeat then
    0
  else
    1 + List.length clicks - Click.countStarted beat clicks

setTime : Float -> Player -> Maybe Player
setTime now player =
  let
    beat = Metro.getBeat player.metro now
  in
    let
      newUnfinishedCount =
        countUnfinished player.clicks player.stopBeat beat
    in
      if newUnfinishedCount /= player.unfinishedCount then
        Just
          { player
          | unfinishedCount = newUnfinishedCount
          }
      else
        Nothing

stop : Float -> Player -> Player
stop now player =
  let
    beat = Metro.getBeat player.metro now
  in
    { player
    | clicks = Click.keepBefore beat player.clicks
    , stopBeat = beat
    , unfinishedCount = 0
    }
