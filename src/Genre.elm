module Genre exposing (Genre(..), init, isQuantized, defaultBpm)

import PlayStyle exposing (PlayStyle)
import StrumPattern exposing (StrumPattern)

type Genre
  = Arp
  | Basic
  | Indie
  | Modern
  | Pad

init : PlayStyle -> StrumPattern -> Genre
init playStyle strumPattern =
  case playStyle of
    PlayStyle.Arpeggio ->
      Arp
    PlayStyle.StrumPattern ->
      case strumPattern of
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

defaultBpm : Genre -> Float
defaultBpm genre =
  case genre of
    Arp ->
      85
    Basic ->
      65
    Indie ->
      85
    Modern ->
      95
    Pad ->
      Debug.todo
        "Genre.defaultBpm: Switching to pad mode should not change tempo"
