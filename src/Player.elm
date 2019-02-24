module Player exposing
  ( Player, start, setBpm, setGenre, addClick, setTime, stop
  , currentIdChord, scheduled, playing, willChange
  , sequence, sequenceAtTime, play
  )

import Arpeggio
import Chord exposing (Chord)
import Click exposing (Click)
import Clip exposing (Clip)
import Genre exposing (Genre)
import IdChord exposing (IdChord)
import Metro exposing (Metro)
import Sound exposing (Sound)
import StrumPattern

type alias Player =
  { metro : Metro
  , genre : Genre
  , origin : Int
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
  , origin = 0
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
  let startTime = now + 0.1 in
    { player
    | metro = Metro.setBpm bpm startTime player.metro
    , startBeat = Metro.getBeat player.metro startTime
    , now = now
    }

setGenre : Genre -> Float -> Player -> Player
setGenre newGenre now player =
  if newGenre == Genre.Pad then
      { player
      | genre = newGenre
      , origin = 0
      , stopBeat = infinity
      , startBeat = Metro.getBeat player.metro now
      , now = now
      }
  else
    let newOrigin = Metro.nextOpening player.metro now in
      { player
      | genre = newGenre
      , origin = newOrigin
      , stopBeat = toFloat (newOrigin + 4)
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
          getStopBeat player.genre player.origin startBeat
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

currentIdChord : Player -> IdChord
currentIdChord player =
  case List.drop (player.unfinishedCount - 1) player.clicks of
    [] ->
      Debug.todo
        "Player.currentIdChord: Player is empty, which should be impossible"
    click :: _ ->
      click.idChord

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
    ( List.map
        ( Sound.mapTime
            (max player.now << Metro.getTime player.metro)
        )
        ( List.concat
            [ [ Sound.cancel player.startBeat ]
            , ( playHelp
                  lowestPitch
                  player.genre
                  player.origin
                  player.startBeat
                  player.clicks
                  player.stopBeat
              )
            , ( if isInfinite player.stopBeat then
                  []
                else
                  [ Sound.alarm player.stopBeat ]
              )
            ]
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
            (playHelp lowestPitch genre origin startBeat rest click.time)
          else
            []
        )
        ( List.filter
            ( Sound.timeInRange
                (max startBeat click.time)
                stopBeat
            )
            ( (::)
                (Sound.mute click.time)
                ( .sounds
                    (playClick lowestPitch genre stopBeat click rest origin)
                )
            )
        )

playClick : Int -> Genre -> Float -> Click -> List Click -> Int -> Clip
playClick lowestPitch genre stopBeat click rest origin =
  case genre of
    Genre.Arp ->
      let
        intStart = beatFloor origin 2 click.time
        intStop = beatCeiling origin 2 stopBeat
        startLow = Arpeggio.startLow lowestPitch click.idChord.chord
        startHigh = Arpeggio.startHigh lowestPitch click.idChord.chord
      in
        Clip.concat
          [ Clip.startAt (toFloat intStart)
          , Clip.repeat
              ((origin - intStart) // 2)
              startLow -- fill the time before origin
          , if
              intStart > origin &&
                containsTime (toFloat (intStart - 2)) rest
            then
              startHigh -- chord was changed less than one measure ago
            else
              startLow
          , Clip.repeat
              ((intStop - max origin intStart) // 4)
              (Clip.append startHigh startLow)
          ]
    Genre.Basic ->
      let
        intStart = beatFloor origin 2 click.time
        intStop = beatCeiling origin 2 stopBeat
      in
        Clip.concat
          [ Clip.startAt (toFloat intStart)
          , Clip.repeat
              ((intStop - intStart) // 2)
              (StrumPattern.basic lowestPitch click.idChord.chord)
          ]
    Genre.Indie ->
      let
        intStart = beatFloor origin 2 click.time
        intStop = beatCeiling origin 2 stopBeat
      in
        Clip.concat
          [ Clip.startAt (toFloat intStart)
          , Clip.repeat
              ((intStop - intStart) // 2)
              (StrumPattern.indie lowestPitch click.idChord.chord)
          ]
    Genre.Modern ->
      let
        intStart = beatFloor origin 4 click.time
        intStop = beatCeiling origin 4 stopBeat
      in
        Clip.concat
          [ Clip.startAt (toFloat intStart)
          , Clip.repeat
              ((intStop - intStart) // 4)
              (StrumPattern.modern lowestPitch click.idChord.chord)
          ]
    Genre.Pad ->
      { sounds =
          List.map
            (Sound.pad click.time)
            (Chord.toPitches lowestPitch click.idChord.chord)
      , stop = infinity
      }

containsTime : Float -> List Click -> Bool
containsTime time clicks =
  case clicks of
    [] ->
      False
    click :: rest ->
      if click.time > time then
        containsTime time rest
      else
        click.time == time

beatFloor : Int -> Int -> Float -> Int
beatFloor origin interval x =
  let i = floor x - origin in
    origin + i - modBy interval i

beatCeiling : Int -> Int -> Float -> Int
beatCeiling origin interval x =
  let i = ceiling x - origin in
    origin + i + modBy interval (interval - i)
