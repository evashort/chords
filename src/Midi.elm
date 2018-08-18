module Midi exposing (fromChords)

import Chord exposing (Chord)

-- https://www.csie.ntu.edu.tw/~r92092/ref/midi/

header : String
header =
  "4d 54 68 64 00000006 0000 0001 0001"

track : String -> String
track data =
  String.join
    " "
    [ "4d 54 72 6b"
    , fixedLength 4 (countBytes data)
    , data
    ]

event : Int -> String -> String
event deltaTime string =
  variableLength deltaTime ++ string

noteOn : Int -> String
noteOn pitch =
  fixedLength 1 pitch ++ "40"

noteOff : Int -> String
noteOff pitch =
  fixedLength 1 pitch ++ "00"

firstChordEvents : Int -> Chord -> String
firstChordEvents lowestPitch chord =
  let
    pitches = Chord.toPitches lowestPitch chord
  in
    case pitches of
      [] ->
        Debug.crash "Midi.fromChords: First chord has no pitches"
      firstPitch :: rest ->
        String.join
          " "
          ( List.concat
              [ [ event 0 ("90" ++ noteOn firstPitch) ]
              , List.map (event 0 << noteOn) rest
              , [ event 4 (noteOff firstPitch) ]
              , List.map (event 0 << noteOff) rest
              ]
          )

chordEvents : Int -> Chord -> String
chordEvents lowestPitch chord =
  let
    pitches = Chord.toPitches lowestPitch chord
  in
    case pitches of
      [] ->
        ""
      firstPitch :: rest ->
        String.join
          " "
          ( List.concat
              [ List.map (event 0 << noteOn) pitches
              , [ event 4 (noteOff firstPitch) ]
              , List.map (event 0 << noteOff) rest
              ]
          )

fromChords : Float -> Int -> List Chord -> String
fromChords bpm lowestPitch chords =
  String.join
    " "
    [ header
    , track
        ( String.join
            " "
            [ event
                0
                ( "ff5103" ++
                    fixedLength 3 (round (60000000 / bpm))
                )
            , event 0 "c000"
            , case chords of
                [] ->
                  ""
                firstChord :: rest ->
                  String.join
                    " "
                    ( firstChordEvents lowestPitch firstChord ::
                        (List.map (chordEvents lowestPitch) rest)
                    )
            , event 0 "ff2f00"
            ]
        )
    ]

fixedLength : Int -> Int -> String
fixedLength length n =
  if n == 0 then
    if length >= 0 then
      String.repeat (2 * length) "0"
    else
      Debug.crash
        ( String.concat
            [ "Midi.fixedLength: Cannot fit "
            , toString n
            , " into "
            , toString length
            , " bytes"
            ]
        )
  else
    (++)
      (fixedLength (length - 1) (n // 256))
      (byteToBase16 (n % 256))

variableLength : Int -> String
variableLength n =
  (++)
    (variableLengthHelp (n // 128))
    (byteToBase16 (n % 128))

variableLengthHelp : Int -> String
variableLengthHelp n =
  if n == 0 then
    ""
  else
    (++)
      (variableLengthHelp (n // 128))
      (byteToBase16 (128 + n % 128))

byteToBase16 : Int -> String
byteToBase16 byte =
  let
    high = byte // 16
    low = byte % 16
  in
    (++)
      (String.slice high (high + 1) "0123456789abcdef")
      (String.slice low (low + 1) "0123456789abcdef")

countBytes : String -> Int
countBytes string =
  let
    length =
      String.length (String.concat (String.split " " string))
  in
    if length % 2 == 1 then
      Debug.crash
        ("Base-16 string contains half byte: \"" ++ string ++ "\"")
    else
      length // 2
