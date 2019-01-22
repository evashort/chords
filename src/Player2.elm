module Player2 exposing
  ( start, setBpm, setGenre, addClick, setTime, stop
  , member, scheduled, playing, willChange
  , sequence, sequenceAtTime, play
  )

import Chord exposing (Chord)
import Click exposing (Click)
import Genre exposing (Genre)
import IdChord exposing (IdChord)
import Metro exposing (Metro)
import Sound exposing (Sound)

type alias Player =
  { metro : Metro
  , genre : Genre
  , genreStart : Int
  , clicks : List Click
  , stopBeat : Float
  , unfinishedCount : Int
  , startBeat : Float
  , now : Float
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
  , startBeat = 0
  , now = click.time
  }

setBpm : Float -> Float -> Player -> Player
setBpm bpm now player =
  { player
  | metro = Metro.setBpm bpm now player.metro
  , startBeat = Metro.getBeat player.metro now
  , now = now
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
    , startBeat = Metro.getBeat player.metro now
    , now = now
    }

addClick : Click -> Player -> Maybe Player
addClick click player =
  let
    startBeat =
      if Genre.isQuantized player.genre then
        toFloat (Metro.nextOpening player.metro click.time)
      else
        Metro.getBeat player.metro click.time
  in
    if startBeat > player.stopBeat then
      Nothing
    else
      let
        newClicks =
          Click.add { click | time = startBeat } player.clicks
        newStopBeat =
          getStopBeat player.genre player.genreStart startBeat
      in
        Just
          { player
          | clicks = newClicks
          , stopBeat = newStopBeat
          , unfinishedCount =
              countUnfinished newClicks newStopBeat startBeat
          , startBeat = startBeat
          , now = click.time
          }

getStopBeat : Genre -> Int -> Float -> Float
getStopBeat genre origin startBeat =
  if genre == Genre.Pad then
    infinity
  else if genre == Genre.Modern || genre == Genre.Basic then
    toFloat (4 + beatCeiling origin 4 startBeat)
  else
    toFloat (4 + beatCeiling origin 2 startBeat)

infinity : Float
infinity = 1 / 0

countUnfinished : List Click -> Float -> Float -> Int
countUnfinished clicks stopBeat beat =
  if beat >= stopBeat then
    0
  else
    1 + List.length clicks - Click.countStarted clicks beat

setTime : Float -> Player -> Maybe Player
setTime now player =
  let beat = Metro.getBeat player.metro now in
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
  let beat = Metro.getBeat player.metro now in
    { player
    | clicks = Click.keepBefore beat player.clicks
    , stopBeat = beat
    , unfinishedCount = 0
    }

member : Player -> Int -> Bool
member player id =
  if player.unfinishedCount <= 0 then
    False
  else
    case List.drop (player.unfinishedCount - 1) player.clicks of
      [] ->
        False
      click :: _ ->
        click.idChord.id == id

scheduled : Player -> Int -> Bool
scheduled player id =
  List.member
    id
    ( List.map
        (.id << .idChord)
        (List.take player.unfinishedCount player.clicks)
    )

playing : Player -> Bool
playing player =
  player.unfinishedCount > 0

willChange : Player -> Bool
willChange player =
  if player.unfinishedCount <= 0 then
    False
  else
    player.unfinishedCount > 1 || player.stopBeat < infinity

sequence : Player -> List Chord
sequence player =
  ( removeDuplicates <<
      List.reverse <<
      List.map (.chord << .idChord) <<
      List.drop (player.unfinishedCount - 1)
  )
    player.clicks

sequenceAtTime : Float -> Player -> List Chord
sequenceAtTime now player =
  let beat = Metro.getBeat player.metro now in
    ( removeDuplicates <<
        List.reverse <<
        List.map (.chord << .idChord) <<
        Click.keepBefore beat
    )
      player.clicks

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

play : Int -> Player -> Cmd msg
play lowestPitch player =
  Sound.play
    ( Sound.mapTime
        (max player.now << Metro.getTime player.metro)
        ( playHelp
            lowestPitch
            player.genre
            player.genreStart
            player.startBeat
            player.clicks
            player.stopBeat
        )
    )

playHelp : Int -> Genre -> Int -> Float -> List Click -> Float -> List Sound
playHelp lowestPitch genre origin startBeat clicks stopBeat =
  case clicks of
    [] ->
      []
    click :: rest ->
      List.append
        ( if click.time > startBeat then
            (playHelp genre origin start rest click.time)
          else
            []
        )
        ( List.filter
            (Sound.timeInRange click.time stopBeat)
            ( case genre of
                Genre.Arp ->
                  let beat = beatFloor origin 2 click.time in
                    Sound.mapTime
                      ((+) (toFloat beat))
                      ( Arp.play
                          (getHighStart origin beat rest)
                          (beat < origin)
                          ((beatCeiling origin 2 stopBeat - beat) // 2)
                          lowestPitch
                          click.idChord.chord
                      )
                Genre.Basic ->
                  let beat = beatFloor origin 4 click.time in
                    Sound.mapTime
                      ((+) (toFloat beat))
                      ( StrumPattern.basic
                          ((beatCeiling origin 4 stopBeat - beat) // 4)
                          lowestPitch
                          click.idChord.chord
                      )
                Genre.Indie ->
                  let beat = beatFloor origin 2 click.time in
                    Sound.mapTime
                      ((+) (toFloat beat))
                      ( StrumPattern.indie
                          ((beatCeiling origin 2 stopBeat - beat) // 2)
                          lowestPitch
                          click.idChord.chord
                      )
                Genre.Modern ->
                  let beat = beatFloor origin 4 click.time in
                    Sound.mapTime
                      ((+) (toFloat beat))
                      ( StrumPattern.modern
                          ((beatCeiling origin 4 stopBeat - beat) // 4)
                          lowestPitch
                          click.idChord.chord
                      )
                Genre.Pad ->
                  Sound.mapTime
                    Arp.pad click.time lowestPitch click.idChord.chord
            )
        )

getHighStart : Int -> Int -> List Click -> Bool
getHighStart origin beat rest =
  if beat <= origin then
    False
  else
    case rest of
      [] ->
        False
      previousClick :: _ ->
        let
          previousBeat =
            max (beatFloor origin 2 previousClick.time) origin
        in
          modBy 4 (beat - previousBeat) == 2

beatFloor : Int -> Int -> Float -> Int
beatFloor origin interval x =
  let i = floor x - origin in
    origin + i - modBy interval i

beatCeiling : Int -> Int -> Float -> Int
beatCeiling origin interval x =
  let i = ceiling x - origin in
    origin + i + modBy interval (interval - i)
