module PlayStatus exposing
  (PlayStatus(..), PlayQueue, hasBorder, hasDashedBorder, hasStopButton)

type PlayStatus
  = Playing PlayQueue
  | Selected Int
  | Cleared

type alias PlayQueue =
  { active : Int
  , stopButton : Bool
  , next : Maybe Int
  }

hasBorder : PlayStatus -> Int -> Bool
hasBorder playStatus id =
  case playStatus of
    Playing playQueue ->
      id == playQueue.active || Just id == playQueue.next
    Selected selection ->
      id == selection
    Cleared ->
      False

hasDashedBorder : PlayStatus -> Int -> Bool
hasDashedBorder playStatus id =
  case playStatus of
    Playing playQueue ->
      Just id == playQueue.next
    _ ->
      False

hasStopButton : PlayStatus -> Int -> Bool
hasStopButton playStatus id =
  case playStatus of
    Playing playQueue ->
      playQueue.stopButton && id == playQueue.active
    _ ->
      False
