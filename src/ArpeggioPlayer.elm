module ArpeggioPlayer exposing (ArpeggioPlayer, play)

import AudioChange exposing (AudioChange(..), Note)
import Chord exposing (Chord)
import PlayStatus exposing (PlayStatus, PlaySegment)

type alias ArpeggioPlayer =
  { openings : List Opening
  }

type alias Opening =
  { endTime : Float
  , beatInterval : Float
  , beat : Float
  , id : Int
  , highStart : Bool
  }

leniency : Float
leniency = 0.05

play :
  Chord -> Int -> Float -> Float -> ArpeggioPlayer ->
    ( ArpeggioPlayer, List AudioChange, List PlaySegment )
play chord id now beatInterval player =
  let
    openingsNotAfter = dropOpeningsAfter now player.openings
  in let
    truncatedOpenings =
      case openingsNotAfter of
        [] -> openingsNotAfter
        opening :: _ ->
          if now <= opening.endTime then openingsNotAfter
          else []
  in let
    ( beat, oldId, highStart ) =
      case truncatedOpenings of
        [] ->
          ( now / beatInterval, -1, False )
        opening :: _ ->
          ( if opening.beatInterval == beatInterval then
              opening.beat
            else
              opening.beat * beatInterval / opening.beatInterval
          , opening.id
          , opening.highStart && opening.id /= id
          )
  in let
    newOpenings =
      [ { endTime = (beat + 4 + leniency) * beatInterval
        , beatInterval = beatInterval
        , beat = beat + 4
        , id = id
        , highStart = False
        }
      , { endTime = (beat + 2 + leniency) * beatInterval
        , beatInterval = beatInterval
        , beat = beat + 2
        , id = id
        , highStart = True
        }
      ] ++
        truncatedOpenings
  in let
    arpeggio =
      if highStart then highArpeggio else lowArpeggio
  in let
    notes = List.map (toNote chord now beatInterval beat) arpeggio
  in let
    startTime = beat * beatInterval
  in let
    schedule =
      List.concat
        [ case truncatedOpenings of
            [] ->
              []
            opening :: _ ->
              [ { status = { active = opening.id, next = id }
                , stop = startTime
                }
              ]
        , [ { status = { active = id, next = -1 }
            , stop = (beat + 4) * beatInterval
            }
          ]
        ]
  in
    ( { player | openings = newOpenings }
    , List.concat
        [ [ if oldId == id then
              CancelFutureNotes { t = startTime, before = now < startTime }
            else
              MuteAllNotes { t = startTime, before = now < startTime }
          , SetDecay 1.5
          ]
        , List.map AddNote notes
        ]
    , PlayStatus.dropBefore now schedule
    )

dropOpeningsAfter : Float -> List Opening -> List Opening
dropOpeningsAfter t openings =
  case openings of
    [] -> openings
    _ :: previousOpenings ->
      case previousOpenings of
        [] -> openings
        previousOpening :: _ ->
          if previousOpening.endTime < t then openings
          else dropOpeningsAfter t previousOpenings

toNote : Chord -> Float -> Float -> Float -> IndexNote -> Note
toNote chord now beatInterval startBeat { offset, beat, i } =
  let pitch = Chord.get chord i + offset in
    { t = max now (beatInterval * (startBeat + beat))
    , f = 440 * 2 ^ (toFloat (pitch - 69) / 12)
    }

type alias IndexNote =
  { offset : Int
  , beat : Float
  , i : Int
  }

highArpeggio : List IndexNote
highArpeggio = IndexNote 24 0 0 :: lowArpeggio

lowArpeggio : List IndexNote
lowArpeggio =
  [ IndexNote 0 0 0
  , IndexNote 0 0.25 1
  , IndexNote 0 0.5 2
  , IndexNote 0 0.75 3
  , IndexNote 0 1 4
  , IndexNote 0 1.25 5
  , IndexNote 0 1.5 3
  , IndexNote 0 1.75 4
  , IndexNote 0 2 0
  , IndexNote 0 2 6
  , IndexNote 0 2.25 1
  , IndexNote 0 2.5 2
  , IndexNote 0 2.75 3
  , IndexNote 0 3 4
  , IndexNote 0 3.25 5
  , IndexNote 0 3.5 3
  , IndexNote 0 3.75 4
  ]
