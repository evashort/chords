module Genre exposing (Genre(..), init, isQuantized)

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
