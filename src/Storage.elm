module Storage exposing (Storage, init, default, code, serialize, deserialize)

import Bpm
import Flag exposing (Flag)
import LowestNote
import Pane exposing (Pane)
import PlayStyle exposing (PlayStyle)
import Ports
import Scale exposing (Scale)
import StrumPattern exposing (StrumPattern)
import Submatches exposing (submatches)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Regex exposing (Regex)

init : Cmd msg
init = Ports.initStorage ()

type alias Storage =
  { bpm : Float
  , key : Scale
  , octave : Int
  , playStyle : PlayStyle
  , strumPattern : StrumPattern
  , strumInterval : Float
  , pane : Pane
  , harmonicMinor : Bool
  , extendedChords : Bool
  , addedToneChords : Bool
  , shortenSequences : Bool
  }

default : Storage
default =
  { bpm = .default <| Bpm.flag
  , key = .default <| Scale.flag
  , octave = .default <| LowestNote.flag
  , playStyle = PlayStyle.Arpeggio
  , strumPattern = StrumPattern.Indie
  , strumInterval = 0.01
  , pane = Pane.ChordsInKey
  , harmonicMinor = False
  , extendedChords = False
  , addedToneChords = False
  , shortenSequences = True
  }

code : Storage -> String
code storage =
  String.concat
    [ codeLine Bpm.flag storage.bpm
    , codeLine Scale.flag storage.key
    , codeLine LowestNote.flag storage.octave
    ]

codeLine : Flag a -> a -> String
codeLine flag value =
  if value == flag.default then
    ""
  else
    flag.key ++ ": " ++ flag.code value ++ "\n"

serialize : Storage -> String
serialize storage =
  Encode.encode 0 (encoder storage)

encoder : Storage -> Encode.Value
encoder storage =
  Encode.object
    [ ( "version"
      , Encode.string (versionString (Version 0 0 0))
      )
    , ( "bpm"
      , Flag.encoder Bpm.flag storage.bpm
      )
    , ( "key"
      , Flag.encoder Scale.flag storage.key
      )
    , ( "octave"
      , Flag.encoder LowestNote.flag storage.octave
      )
    , ( "playStyle"
      , Encode.string (playStyleString storage.playStyle)
      )
    , ( "strumPattern"
      , Encode.string (strumPatternString storage.strumPattern)
      )
    , ( "strumInterval"
      , Encode.float storage.strumInterval
      )
    , ( "pane"
      , Encode.string (paneString storage.pane)
      )
    , ( "harmonicMinor", Encode.bool storage.harmonicMinor )
    , ( "extendedChords", Encode.bool storage.extendedChords )
    , ( "addedToneChords", Encode.bool storage.addedToneChords )
    , ( "shortenSequences", Encode.bool storage.shortenSequences )
    ]

deserialize : String -> Result String Storage
deserialize = Decode.decodeString decoder

decoder : Decoder Storage
decoder =
  Decode.andThen
    decoderHelp
    ( Decode.field
        "version"
        (parseDecoder "version" parseVersion)
    )

decoderHelp : Version -> Decoder Storage
decoderHelp version =
  if version.major > 1 then
    Decode.fail
      ("Incompatible major version: " ++ toString version.major)
  else
    v0Decoder

v0Decoder : Decoder Storage
v0Decoder =
  Pipeline.decode Storage
    |> Pipeline.required
        "bpm"
        (Flag.decoder Bpm.flag)
    |> Pipeline.required
        "key"
        (Flag.decoder Scale.flag)
    |> Pipeline.required
        "octave"
        (Flag.decoder LowestNote.flag)
    |> Pipeline.required
        "playStyle"
        (parseDecoder "play style" parsePlayStyle)
    |> Pipeline.required
        "strumPattern"
        (parseDecoder "strum pattern" parseStrumPattern)
    |> Pipeline.required
        "strumInterval"
        Decode.float
    |> Pipeline.required
        "pane"
        (parseDecoder "pane" parsePane)
    |> Pipeline.required "harmonicMinor" Decode.bool
    |> Pipeline.required "extendedChords" Decode.bool
    |> Pipeline.required "addedToneChords" Decode.bool
    |> Pipeline.required "shortenSequences" Decode.bool

parseDecoder : String -> (String -> Maybe a) -> Decoder a
parseDecoder name parse =
  Decode.andThen
    (failOnNothing ("Could not parse " ++ name) << parse)
    Decode.string

failOnNothing : String -> Maybe a -> Decoder a
failOnNothing message maybe =
  case maybe of
    Just x ->
      Decode.succeed x
    Nothing ->
      Decode.fail message

type alias Version =
  { major : Int
  , minor : Int
  , patch : Int
  }

versionString : Version -> String
versionString version =
  String.join
    "."
    [ toString version.major
    , toString version.minor
    , toString version.patch
    ]

parseVersion : String -> Maybe Version
parseVersion string =
  case submatches versionRegex string of
    [ Just majorString, Just minorString, Just patchString ] ->
      case
        ( String.toInt majorString
        , String.toInt minorString
        , String.toInt patchString
        )
      of
        ( Ok major, Ok minor, Ok patch ) ->
          Just (Version major minor patch)
        _ ->
          Nothing
    _ ->
      Nothing

versionRegex : Regex
versionRegex = Regex.regex "^([0-9]+)\\.([0-9]+)\\.([0-9]+)"

parsePlayStyle : String -> Maybe PlayStyle
parsePlayStyle string =
  case string of
    "Arpeggio" ->
      Just PlayStyle.Arpeggio
    "StrumPattern" ->
      Just PlayStyle.StrumPattern
    "Strum" ->
      Just PlayStyle.Strum
    "Pad" ->
      Just PlayStyle.Pad
    _ ->
      Nothing

playStyleString : PlayStyle -> String
playStyleString playStyle =
  case playStyle of
    PlayStyle.Arpeggio ->
      "Arpeggio"
    PlayStyle.StrumPattern ->
      "StrumPattern"
    PlayStyle.Strum ->
      "Strum"
    PlayStyle.Pad ->
      "Pad"

strumPatternString : StrumPattern -> String
strumPatternString strumPattern =
  case strumPattern of
    StrumPattern.Basic ->
      "Basic"
    StrumPattern.Indie ->
      "Indie"
    StrumPattern.Modern ->
      "Modern"

parseStrumPattern : String -> Maybe StrumPattern
parseStrumPattern string =
  case string of
    "Basic" ->
      Just StrumPattern.Basic
    "Indie" ->
      Just StrumPattern.Indie
    "Modern" ->
      Just StrumPattern.Modern
    _ ->
      Nothing

paneString : Pane -> String
paneString pane =
  case pane of
    Pane.ChordsInKey ->
      "ChordsInKey"
    Pane.Circle ->
      "Circle"
    Pane.History ->
      "History"

parsePane : String -> Maybe Pane
parsePane string =
  case string of
    "ChordsInKey" ->
      Just Pane.ChordsInKey
    "Circle" ->
      Just Pane.Circle
    "History" ->
      Just Pane.History
    _ ->
      Nothing
