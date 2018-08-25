module Storage exposing
  (Storage, default, delete, save, decode, setVolume, setStrumInterval)

import Pane exposing (Pane)
import PlayStyle exposing (PlayStyle)
import Ports
import StrumPattern exposing (StrumPattern)
import Submatches exposing (submatches)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Regex exposing (Regex)

type alias Storage =
  { playStyle : PlayStyle
  , volume : Int
  , strumPattern : StrumPattern
  , strumInterval : Float
  , pane : Pane
  , harmonicMinor : Bool
  , addedToneChords : Bool
  , shortenSequences : Bool
  , startEmpty : Bool
  , unsavedWarning : Bool
  }

default : Storage
default =
  { playStyle = PlayStyle.Arpeggio
  , volume = 30
  , strumPattern = StrumPattern.Indie
  , strumInterval = 0.01
  , pane = Pane.Search
  , harmonicMinor = False
  , addedToneChords = False
  , shortenSequences = True
  , startEmpty = False
  , unsavedWarning = False
  }

delete : Cmd msg
delete = Ports.deleteStorage ()

save : Storage -> Cmd msg
save storage =
  Ports.saveStorage (encode storage)

encode : Storage -> Encode.Value
encode storage =
  Encode.object
    [ ( "version"
      , Encode.string (versionString (Version 1 0 0))
      )
    , ( "playStyle"
      , Encode.string (playStyleString storage.playStyle)
      )
    , ( "volume"
      , Encode.int storage.volume
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
    , ( "addedToneChords", Encode.bool storage.addedToneChords )
    , ( "shortenSequences", Encode.bool storage.shortenSequences )
    , ( "startEmpty", Encode.bool storage.startEmpty )
    , ( "unsavedWarning", Encode.bool storage.unsavedWarning )
    ]

decode : Encode.Value -> Result String (Maybe Storage)
decode blob =
  case Decode.decodeValue (Decode.null Nothing) blob of
    Err _ ->
      Result.map Just (Decode.decodeValue decoder blob)
    nothingResult ->
      nothingResult

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
    v1_0Decoder

v1_0Decoder : Decoder Storage
v1_0Decoder =
  Pipeline.decode Storage
    |> Pipeline.required
        "playStyle"
        (parseDecoder "play style" parsePlayStyle)
    |> Pipeline.required "volume" Decode.int
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
    |> Pipeline.required "addedToneChords" Decode.bool
    |> Pipeline.required "shortenSequences" Decode.bool
    |> Pipeline.required "startEmpty" Decode.bool
    |> Pipeline.required "unsavedWarning" Decode.bool

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
    "Strum" ->
      Just PlayStyle.Strum
    "Pad" ->
      Just PlayStyle.Pad
    "Arpeggio" ->
      Just PlayStyle.Arpeggio
    "StrumPattern" ->
      Just PlayStyle.StrumPattern
    "Silent" ->
      Just PlayStyle.Silent
    _ ->
      Nothing

playStyleString : PlayStyle -> String
playStyleString playStyle =
  case playStyle of
    PlayStyle.Strum ->
      "Strum"
    PlayStyle.Pad ->
      "Pad"
    PlayStyle.Arpeggio ->
      "Arpeggio"
    PlayStyle.StrumPattern ->
      "StrumPattern"
    PlayStyle.Silent ->
      "Silent"

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
    Pane.Search ->
      "Search"
    Pane.ChordsInKey ->
      "ChordsInKey"
    Pane.Circle ->
      "Circle"
    Pane.History ->
      "History"
    Pane.Settings ->
      "Settings"

parsePane : String -> Maybe Pane
parsePane string =
  case string of
    "Search" ->
      Just Pane.Search
    "ChordsInKey" ->
      Just Pane.ChordsInKey
    "Circle" ->
      Just Pane.Circle
    "History" ->
      Just Pane.History
    "Settings" ->
      Just Pane.Settings
    _ ->
      Nothing

setStrumInterval : Storage -> String -> Storage
setStrumInterval storage strumIntervalString =
  case String.toFloat strumIntervalString of
    Ok strumInterval ->
      { storage | strumInterval = 0.001 * strumInterval }
    Err _ ->
      Debug.crash
        ( "Storage.setStrumInterval: Bad strum interval: " ++
            strumIntervalString
        )

setVolume : Storage -> String -> Storage
setVolume storage volumeString =
  case String.toInt volumeString of
    Ok volume ->
      { storage | volume = volume }
    Err _ ->
      Debug.crash
        ("Storage.setVolume: Bad volume: " ++ volumeString)
