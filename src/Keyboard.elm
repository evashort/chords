module Keyboard exposing
  ( Keyboard, LastSound(..), ChordSource(..), init, status, getId, Msg(..)
  , update, view
  )

import AudioChange exposing (AudioChange(..))
import Chord exposing (Chord)
import Colour
import CustomEvents exposing
  ( isAudioTimeButton, onClickWithAudioTime
  , isAudioTimeInput, onInputWithAudioTime, onIntInputWithAudioTime
  )
import Harp exposing
  ( viewBoxLeft, viewBoxRight, isWhiteKey, neckLeft, headLeft
  , borderWidth, headWidth, scale
  )
import IdChord exposing (IdChord)
import Name
import Note exposing (Note)
import Path
import Player exposing (Player)
import PlayStatus exposing (PlayStatus)
import Ports exposing (Pluck)

import Html exposing (Html, span, text, input, button)
import Html.Attributes as Attributes exposing (attribute, style, class, id)
import Html.Events exposing (onInput, onClick)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy

type alias Keyboard =
  { player : Player
  , lastSound : LastSound
  , source : ChordSource
  , customCode : String
  , customOctave : Int
  }

type LastSound
  = PlayerSound
  | DirtyPluck
  | Clean

type ChordSource
  = LastPlayed
  | ThisChord IdChord
  | CustomChord
  | NoChord

init : Keyboard
init =
  { player = Player.init
  , lastSound = Clean
  , source = NoChord
  , customCode = ""
  , customOctave = 0
  }

getCode : Keyboard -> String
getCode keyboard =
  case keyboard.source of
    LastPlayed ->
      case Player.lastPlayed keyboard.player of
        Nothing ->
          ""
        Just idChord ->
          Name.code idChord.chord
    ThisChord idChord ->
      Name.code idChord.chord
    CustomChord ->
      keyboard.customCode
    NoChord ->
      ""

getChord : Keyboard -> Maybe Chord
getChord keyboard =
  case keyboard.source of
    LastPlayed ->
      Maybe.map
        .chord
        (Player.lastPlayed keyboard.player)
    ThisChord idChord ->
      Just idChord.chord
    CustomChord ->
      Chord.fromCodeExtended keyboard.customCode
    NoChord ->
      Nothing

getOctave : Keyboard -> Int
getOctave keyboard =
  if keyboard.source == CustomChord then
    keyboard.customOctave
  else
    0

status : Bool -> Keyboard -> PlayStatus
status showSilent keyboard =
  case keyboard.source of
    LastPlayed ->
      Player.status showSilent keyboard.player
    ThisChord idChord ->
      if showSilent then
        PlayStatus.Selected idChord.id
      else
        PlayStatus.Cleared
    _ ->
      PlayStatus.Cleared

getId : Keyboard -> Maybe Int
getId keyboard =
  case keyboard.source of
    LastPlayed ->
      Maybe.map
        .id
        (Player.lastPlayed keyboard.player)
    ThisChord idChord ->
      Just idChord.id
    _ ->
      Nothing

type Msg
  = ShowCustomChord (Bool, Float)
  | SetCode (String, Float)
  | SetOctave (Int, Float)
  | AddPitch (Int, Int, Float)
  | RemovePitch (Int, Int, Float)
  | HarpPlucked Pluck
  | AddWord String

update : Msg -> Keyboard -> (Keyboard, Cmd Msg)
update msg keyboard =
  case msg of
    ShowCustomChord ( showCustomChord, now ) ->
      ( { keyboard
        | player = Player.stop now keyboard.player
        , lastSound = Clean
        , source =
            if showCustomChord then
              CustomChord
            else
              NoChord
        }
      , if keyboard.lastSound == PlayerSound then
          AudioChange.perform [ Mute now ]
        else
          Cmd.none
      )

    SetCode ( code, now ) ->
      ( { keyboard
        | player = Player.stop now keyboard.player
        , lastSound = Clean
        , source =
            if code == "" then
              NoChord
            else
              CustomChord
        , customCode = code
        , customOctave = getOctave keyboard
        }
      , if keyboard.lastSound == PlayerSound then
          AudioChange.perform [ Mute now ]
        else
          Cmd.none
      )

    SetOctave ( octave, now ) ->
      ( { keyboard
        | player = Player.stop now keyboard.player
        , lastSound = Clean
        , source = CustomChord
        , customCode = getCode keyboard
        , customOctave = octave
        }
      , if keyboard.lastSound == PlayerSound then
          AudioChange.perform [ Mute now ]
        else
          Cmd.none
      )

    AddPitch ( lowestPitch, pitch, now ) ->
      let
        pitchSet =
          Chord.toPitchSet
            lowestPitch
            (getOctave keyboard)
            (getChord keyboard)
      in let
        newPitchSet =
          Set.filter
            ( inRange
                (pitch - Chord.maxRange)
                (pitch + Chord.maxRange)
            )
            (Set.insert pitch pitchSet)
      in let
        ( newChord, newOctave ) =
          case Chord.fromPitchSet lowestPitch newPitchSet of
            Just x ->
              x
            Nothing ->
              Debug.crash
                "Keyboard.update: Pitch set empty after inserting pitch"
      in
        ( { keyboard
          | player = Player.stop now keyboard.player
          , lastSound = Clean
          , source = CustomChord
          , customCode = Name.codeExtended newChord
          , customOctave = newOctave
          }
        , let
            changes =
              [ AddPianoNote
                  { v = 1
                  , t = now
                  , f = pitchFrequency pitch
                  }
              ]
          in
            if keyboard.lastSound == Clean then
              AudioChange.perform changes
            else
              AudioChange.perform (Mute now :: changes)
        )

    RemovePitch ( lowestPitch, pitch, now ) ->
      let
        pitchSet =
          Chord.toPitchSet
            lowestPitch
            (getOctave keyboard)
            (getChord keyboard)
      in let
        newPitchSet =
          Set.remove pitch pitchSet
      in
        ( case Chord.fromPitchSet lowestPitch newPitchSet of
            Just ( newChord, newOctave ) ->
              { keyboard
              | player = Player.stop now keyboard.player
              , lastSound = Clean
              , source = CustomChord
              , customCode = Name.codeExtended newChord
              , customOctave = newOctave
              }
            Nothing ->
              { keyboard
              | player = Player.stop now keyboard.player
              , lastSound = Clean
              , source = NoChord
              , customCode = ""
              , customOctave = 0
              }
        , if keyboard.lastSound == Clean then
            AudioChange.perform
              [ NoteOff
                  { v = 1
                  , t = now
                  , f = pitchFrequency pitch
                  }
              ]
          else
            AudioChange.perform [ Mute now ]
        )

    HarpPlucked pluck ->
      let
        changes =
          (++)
            ( List.map
                ( NoteOff <<
                    Note 1 pluck.now <<
                      pitchFrequency
                )
                pluck.mutes
            )
            ( List.map
                ( AddGuitarNote <<
                    Note 1 pluck.now <<
                      pitchFrequency
                )
                pluck.pitches
            )
      in
        ( { keyboard
          | player = Player.stop pluck.now keyboard.player
          , lastSound = Clean
          }
        , if keyboard.lastSound /= Clean then
            AudioChange.perform (Mute pluck.now :: changes)
          else
            AudioChange.perform changes
        )

    AddWord _ ->
      ( keyboard, Cmd.none )

inRange : Int -> Int -> Int -> Bool
inRange low high x =
  low <= x && x <= high

pitchFrequency : Int -> Float
pitchFrequency pitch =
  440 * 2 ^ (toFloat (pitch - 69) / 12)

view : String -> Int -> Int -> Keyboard -> Html Msg
view gridArea tonic lowestPitch keyboard =
  let
    maybeChord = getChord keyboard
    octave = getOctave keyboard
  in let
    maxOctave =
      case maybeChord of
        Nothing ->
          0
        Just chord ->
          let
            rootPitch =
              (chord.root - lowestPitch) % 12 + lowestPitch
            highestOffset =
              case List.reverse chord.flavor of
                [] ->
                  0
                flavorPitch :: _ ->
                  flavorPitch
          in let
            highestPitch = rootPitch + highestOffset
            maxPitch = lowestPitch + 11 + Chord.maxRange
          in let
            maxTransposition = maxPitch - highestPitch
          in
            (maxTransposition - maxTransposition % 12) // 12
    pitchSet =
      Chord.toPitchSet lowestPitch octave maybeChord
  in
    span
      [ id gridArea
      , style
          [ ( "grid-area", gridArea )
          , ( "margin-top", "5px" )
          ]
      ]
      [ Harp.view
          tonic
          lowestPitch
          (lowestPitch + 11 + Chord.maxRange)
          pitchSet
      , viewKeys
          tonic
          lowestPitch
          pitchSet
      , span
          [ style
              [ ( "display", "block" )
              ]
          ]
          [ text "Chord "
          , input
              [ class "textInput"
              , Attributes.type_ "text"
              , isAudioTimeInput True
              , onInputWithAudioTime SetCode
              , Attributes.value (getCode keyboard)
              ]
              []
          , button
              [ class "button"
              , case Maybe.map Name.code maybeChord of
                  Just "unknown" ->
                    Attributes.disabled True
                  Just word ->
                    onClick (AddWord word)
                  Nothing ->
                    Attributes.disabled True
              ]
              [ text "Add"
              ]
          , text " Octave "
          , input
              [ class "numberInput"
              , Attributes.type_ "number"
              , Attributes.disabled
                  (maxOctave <= 0 && octave == 0)
              , isAudioTimeInput True
              , onIntInputWithAudioTime octave SetOctave
              , Attributes.value (toString octave)
              , Attributes.min "0"
              , Attributes.max (toString maxOctave)
              , style
                  [ ( "width", "5ch" )
                  ]
              ]
              []
          ]
      ]

-- the origin is the top left corner of middle C,
-- not including its border
viewKeys : Int -> Int -> Set Int -> Html Msg
viewKeys tonic lowestPitch pitchSet =
  let
    highestPitch = lowestPitch + 11 + Chord.maxRange
  in let
    left = viewBoxLeft lowestPitch
    right = viewBoxRight highestPitch
  in let
    width = right - left
    height = fullHeight
  in
    Svg.svg
      [ SA.width (toString width)
      , SA.height (toString height)
      , SA.viewBox
          ( String.join
              " "
              [ toString left
              , "0"
              , toString width
              , toString height
              ]
          )
      , style
          [ ( "display", "block" )
          ]
      ]
      ( List.concat
          [ [ Svg.defs
                []
                [ blackKeyGradient
                , whiteKeyGradient
                , specularGradient
                ]
            , Svg.rect
                [ SA.x (toString left)
                , SA.y "0"
                , SA.width (toString width)
                , SA.height (toString height)
                , SA.fill "black"
                ]
                []
            ]
          , [ Svg.map
                (pitchMsg lowestPitch pitchSet)
                (Svg.Lazy.lazy2 viewStaticKeys tonic lowestPitch)
            ]
          , List.map
              (Svg.map (pitchMsg lowestPitch pitchSet))
              ( List.concatMap
                  (viewKey tonic lowestPitch highestPitch True)
                  ( List.filter
                      (inRange lowestPitch highestPitch)
                      (Set.toList pitchSet)
                  )
              )
          , [ Svg.text_
                [ style
                    [ ( "pointer-events", "none" )
                    ]
                , SA.textAnchor "middle"
                , SA.x
                    (toString (0.5 * (headWidth - borderWidth)))
                , SA.y
                    ( toString
                        ( fullHeight - borderWidth -
                            0.25 * (headWidth - borderWidth)
                        )
                    )
                ]
                [ Svg.text "C4"
                ]
            ]
          ]
      )

pitchMsg : Int -> Set Int -> (Int, Float) -> Msg
pitchMsg lowestPitch pitchSet ( pitch, now ) =
  if Set.member pitch pitchSet then
    RemovePitch (lowestPitch, pitch, now)
  else
    AddPitch (lowestPitch, pitch, now)

viewStaticKeys : Int -> Int -> Svg (Int, Float)
viewStaticKeys tonic lowestPitch =
  let
    highestPitch = lowestPitch + 11 + Chord.maxRange
  in
    Svg.g
      []
      ( List.concatMap
          (viewKey tonic lowestPitch highestPitch False)
          (List.range lowestPitch highestPitch)
      )

viewKey : Int -> Int -> Int -> Bool -> Int -> List (Svg (Int, Float))
viewKey tonic lowestPitch highestPitch selected pitch =
  let
    commonAttributes =
      if selected then
        []
      else
        [ isAudioTimeButton True
        , onClickWithAudioTime ((,) pitch)
        ]
  in
    if isWhiteKey pitch then
      let
        path = whitePath lowestPitch highestPitch pitch
      in
        [ Svg.path
            ( [ style
                  [ if selected then
                      ( "pointer-events", "none" )
                    else
                      ( "cursor", "pointer" )
                  ]
              , SA.fill
                  ( if selected then
                      Colour.pitchBg tonic pitch
                    else
                      "white"
                  )
              , SA.d path
              ] ++
                commonAttributes
            )
            []
        ] ++
          ( if selected then
              [ Svg.path
                  [ style
                      [ ( "pointer-events", "none" )
                      ]
                  , SA.fill "url(#whiteKeyGradient)"
                  , SA.d path
                  ]
                  []
              ]
            else
              []
          )
    else
      [ Svg.rect
          ( [ style
                [ if selected then
                    ( "pointer-events", "none" )
                  else
                    ( "cursor", "pointer" )
                ]
            , SA.fill
                ( if selected then
                    Colour.pitchBg tonic pitch
                  else
                    "black"
                )
            , SA.strokeWidth (toString borderWidth)
            , SA.strokeLinejoin "round"
            , SA.x (toString (neckLeft pitch))
            , SA.y "0"
            , SA.width (toString blackWidth)
            , SA.height (toString (blackHeight - borderWidth))
            ] ++
              commonAttributes
          )
          []
      , Svg.path
          [ style
              [ ( "pointer-events", "none" )
              ]
          , SA.fill "url(#blackKeyGradient)"
          , SA.opacity (toString (leftSideOpacity selected))
          , SA.d (leftSidePath pitch)
          ]
          []
      , Svg.path
          [ style
              [ ( "pointer-events", "none" )
              ]
          , SA.fill "url(#specularGradient)"
          , SA.opacity (toString (specularOpacity selected))
          , SA.d (specularPath pitch)
          ]
          []
      , Svg.path
          [ style
              [ ( "pointer-events", "none" )
              ]
          , SA.fill "url(#blackKeyGradient)"
          , SA.opacity (toString fingerOpacity)
          , SA.d (fingerPath pitch)
          ]
          []
      , Svg.path
          [ style
              [ ( "pointer-events", "none" )
              ]
          , SA.fill "url(#blackKeyGradient)"
          , SA.opacity (toString (hillOpacity selected))
          , SA.d (hillPath pitch)
          ]
          []
      ]

whitePath : Int -> Int -> Int -> String
whitePath lowestPitch highestPitch pitch =
  String.join
    " "
    [ Path.bigM
        ( if pitch == lowestPitch then
            headLeft pitch
          else
            neckLeft pitch
        )
        0
    , Path.bigV blackHeight
    , Path.bigH (headLeft pitch)
    , Path.bigV (fullHeight - borderWidth - borderRadius)
    , Path.a
        borderRadius borderRadius
        90 False False
        borderRadius borderRadius
    , Path.h (headWidth - borderWidth - 2 * borderRadius)
    , Path.a
        borderRadius borderRadius
        90 False False
        borderRadius -borderRadius
    , Path.bigV blackHeight
    , Path.bigH
        ( if pitch == highestPitch then
            headLeft pitch + headWidth - borderWidth
          else
            neckLeft (pitch + 1) - borderWidth
        )
    , Path.bigV 0
    , Path.bigZ
    ]

fingerPath : Int -> String
fingerPath pitch =
  String.join
    " "
    [ Path.bigM (neckLeft pitch + sideWidth) 0
    , Path.bigV
        (blackHeight - borderWidth - hillHeight - nailHeight)
    , Path.c
        0 (nailHeight / 0.75)
        (blackWidth - 2 * sideWidth) (nailHeight / 0.75)
        (blackWidth - 2 * sideWidth) 0
    , Path.bigV 0
    , Path.bigZ
    ]

leftSidePath : Int -> String
leftSidePath pitch =
  String.join
    " "
    [ Path.bigM (neckLeft pitch) 0
    , Path.bigV (blackHeight - borderWidth)
    , Path.c
        (hillHeight / 1.5 / hillSlope) (-hillHeight / 1.5)
        (0.25 * blackWidth + hillHeight / 3 / hillSlope) (-hillHeight)
        (0.5 * blackWidth) (-hillHeight)
    , Path.c
        (-0.25 * blackWidth + 0.5 * sideWidth) 0
        (-0.5 * blackWidth + sideWidth) (-nailHeight / 3)
        (-0.5 * blackWidth + sideWidth) (-nailHeight)
    , Path.bigV 0
    , Path.bigZ
    ]

specularPath : Int -> String
specularPath pitch =
  String.join
    " "
    [ Path.bigM (neckLeft pitch) (blackHeight - borderWidth - specularHeight)
    , Path.bigV (blackHeight - borderWidth)
    , Path.c
        (hillHeight / 1.5 / hillSlope) (-hillHeight / 1.5)
        (0.25 * blackWidth + hillHeight / 3 / hillSlope) (-hillHeight)
        (0.5 * blackWidth) (-hillHeight)
    , Path.c
        (-0.25 * blackWidth + 0.5 * sideWidth) 0
        (-0.5 * blackWidth + sideWidth) (-nailHeight / 3)
        (-0.5 * blackWidth + sideWidth) (-nailHeight)
    , Path.bigV (blackHeight - borderWidth - specularHeight)
    , Path.bigZ
    ]

hillPath : Int -> String
hillPath pitch =
  String.join
    " "
    [ Path.bigM (neckLeft pitch + blackWidth) 0
    , Path.bigV (blackHeight - borderWidth)
    , Path.h -blackWidth
    , Path.partialC
        rightShineT
        (hillHeight / 0.75 / hillSlope) (-hillHeight / 0.75)
        (blackWidth - hillHeight / 0.75 / hillSlope) (-hillHeight / 0.75)
        blackWidth 0
    , Path.bigV 0
    , Path.bigZ
    ]

-- white keys have rounded corners at the bottom
-- the radius is measured at the edge of the white area,
-- inside the border
borderRadius : Float
borderRadius = 0.75 * scale

blackHeight : Float -- includes one border width
blackHeight = 15.5 * scale

fullHeight : Float -- includes one border width
fullHeight = 24 * scale

-- black key lighting parameters (these don't include any border width)
blackWidth : Float
blackWidth = 4 * headWidth / 7 - borderWidth

nailHeight : Float
nailHeight = 0.27 * blackWidth

hillHeight : Float
hillHeight = 0.44 * blackWidth

hillSlope : Float
hillSlope = 7

sideWidth : Float
sideWidth = 0.07 * blackWidth

rightShineT : Float
rightShineT = 1 - 0.12

specularHeight : Float
specularHeight = 2 * blackWidth

fingerOpacity : Float
fingerOpacity = 0.28

hillOpacity : Bool -> Float
hillOpacity selected =
  if selected then 0.6 else 0.46

leftSideOpacity : Bool -> Float
leftSideOpacity selected =
  if selected then 1 else 0.67

specularOpacity : Bool -> Float
specularOpacity selected =
  if selected then 1 else 0.4

blackKeyStartOpacity : Float
blackKeyStartOpacity = 0.3

blackKeyGradient : Svg msg
blackKeyGradient =
  Svg.linearGradient
    [ SA.id "blackKeyGradient"
    , SA.y1 "0%"
    , SA.y2 "100%"
    , SA.x1 "50%"
    , SA.x2 "50%"
    ]
    [ Svg.stop
        [ SA.offset "0%"
        , style
            [ ( "stop-color", "white" )
            , ( "stop-opacity", toString blackKeyStartOpacity )
            ]
        ]
        []
    , Svg.stop
        [ SA.offset "100%"
        , style
            [ ( "stop-color", "white" )
            , ( "stop-opacity", "1" )
            ]
        ]
        []
    ]

whiteKeyGradient : Svg msg
whiteKeyGradient =
  let
    startOpacity = blackKeyStartOpacity * fingerOpacity
    slope =
      (1 - blackKeyStartOpacity) * fingerOpacity /
        (blackHeight - borderWidth - hillHeight)
  in let
    endOpacity =
      startOpacity + slope * (fullHeight - borderWidth)
  in
    Svg.linearGradient
      [ SA.id "whiteKeyGradient"
      , SA.y1 "0%"
      , SA.y2 "100%"
      , SA.x1 "50%"
      , SA.x2 "50%"
      ]
      [ Svg.stop
          [ SA.offset "0%"
          , style
              [ ( "stop-color", "white" )
              , ( "stop-opacity", toString startOpacity )
              ]
          ]
          []
      , Svg.stop
          [ SA.offset "100%"
          , style
              [ ( "stop-color", "white" )
              , ( "stop-opacity", toString endOpacity )
              ]
          ]
          []
      ]

specularGradient : Svg msg
specularGradient =
  Svg.radialGradient
    [ SA.id "specularGradient"
    , SA.cx "7.1%"
    , SA.cy "76%"
    , SA.r "7%"
    , SA.fx "7.1%"
    , SA.fy "76%"
    , SA.gradientTransform "scale(4 1)"
    ]
    [ Svg.stop
        [ SA.offset "0%"
        , style
            [ ( "stop-color", "white" )
            , ( "stop-opacity", "1" )
            ]
        ]
        []
    , Svg.stop
        [ SA.offset "100%"
        , style
            [ ( "stop-color", "white" )
            , ( "stop-opacity", "0" )
            ]
        ]
        []
    ]
