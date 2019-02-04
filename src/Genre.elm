module Genre exposing (Genre(..), fromStorage, isQuantized, defaultBpm)

import PlayStyle exposing (PlayStyle)
import Storage exposing (Storage)
import StrumPattern exposing (StrumPattern)

type Genre
  = Arp
  | Basic
  | Indie
  | Modern
  | Pad

fromStorage : Storage -> Genre
fromStorage storage =
  case storage.playStyle of
    PlayStyle.Arpeggio ->
      Arp
    PlayStyle.StrumPattern ->
      case storage.strumPattern of
        StrumPattern.Basic ->
          Basic
        StrumPattern.Indie ->
          Indie
        StrumPattern.Modern ->
          Modern
    PlayStyle.Pad ->
      Pad

isQuantized : Genre -> Bool
isQuantized genre =
  genre /= Pad

defaultBpm : Storage -> Float
defaultBpm storage =
  case fromStorage storage of
    Basic ->
      65
    Modern ->
      95
    _ ->
      85
