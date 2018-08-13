module Tour exposing
  ( shadowBuffet, padSong, paneShadow
  , Tour, init, view, viewPageOptions, scrollIntoView
  )

import Buffet exposing (Buffet)
import Pane exposing (Pane)
import Path
import Ports
import Song exposing (Song)

import Html exposing
  (Html, text, span, div, p, ul, li, sup, code, mark, em, button, option)
import Html.Attributes as Attributes exposing
  (style, class, id, disabled, value, selected)
import Html.Events exposing (onClick)

import Svg exposing (svg, path, filter, feGaussianBlur)
import Svg.Attributes as SA

shadowBuffet : Tour -> Buffet -> Buffet
shadowBuffet tour buffet =
  if
    tour.visible &&
      getTitle tour.pageNumber == "Replacements"
  then
    { suggestions =
        [ { swatches =
              [ { fg = "#000000"
                , bg = "rgb(255, 175, 179)"
                , s = "Bb"
                }
              ]
          , ranges = []
          }
        ]
    , lenses = []
    }
  else
    buffet

padSong : Tour -> Song -> Song
padSong tour song =
  if tour.visible then
    let
      newLength =
        case getTitle tour.pageNumber of
          "Chord progression" -> 1
          _ -> 0
    in
      (++)
        song
        ( List.repeat
            (newLength - List.length song)
            [ Nothing ]
        )
  else
    song

paneShadow : Tour -> Maybe Pane
paneShadow tour =
  if tour.visible then
    case getTitle tour.pageNumber of
      "Search results" -> Just Pane.Search
      "Settings" -> Just Pane.Settings
      _ -> Nothing
  else
    Nothing

type alias Tour =
  { visible : Bool
  , pageNumber : Int
  }

init : Tour
init =
  { visible = False
  , pageNumber = 1
  }

view : Bool -> Tour -> Html Tour
view mac tour =
  if tour.visible then
    if mac && getTitle tour.pageNumber == "Text-based parameters" then
      macPageView
    else
      case List.drop (tour.pageNumber - 1) pageViews of
        [] ->
          Debug.crash
            ( "Tour.view: Page number out of range: " ++
                toString tour.pageNumber
            )
        pageView :: _ ->
          pageView
  else
    span
      [ style
          [ ( "display", "none" )
          ]
      ]
      []

macPageNumber : Int
macPageNumber = 5

getTitle : Int -> String
getTitle pageNumber =
  case List.drop (pageNumber - 1) pages of
    [] ->
      Debug.crash
        ( "Tour.getTitle: Page number out of range: " ++
            toString pageNumber
        )
    page :: _ ->
      page.title

macPageView : Html Tour
macPageView =
  viewPage
    (List.length pages)
    (macPageNumber - 1)
    (macDependentPage True)

pageViews : List (Html Tour)
pageViews =
  List.indexedMap
    (viewPage (List.length pages))
    pages

viewPage : Int -> Int -> Page -> Html Tour
viewPage pageCount pageIndex page =
  span
    [ style
        [ ( "grid-area", page.gridArea )
        , ( "position", "absolute" )
        , ( "z-index", "3" )
        , case ( page.orientation, page.anchor ) of
            ( Above, BottomOf ) ->
              ( "bottom"
              , "calc(32px + " ++ toString page.y ++ "em)"
              )
            ( Below, TopOf ) ->
              ( "top"
              , "calc(32px + " ++ toString page.y ++ "em)"
              )
            ( Above, TopOf ) ->
              ( "bottom"
              , "calc(100% + 32px + " ++ toString page.y ++ "em)"
              )
            ( Below, BottomOf ) ->
              ( "top"
              , "calc(100% + 32px + " ++ toString page.y ++ "em)"
              )
        , ( "left"
          , if page.justify == CenterAt then
              toString (page.x - 15) ++ "em"
            else
              toString page.x ++ "em"
          )
        , ( "background", "white" )
        , ( "border", "1px solid" )
        , ( "border-radius", "16px" )
        , ( "width", "30em" )
        , ( "white-space", "normal" )
        , ( "line-height", "normal" )
        , ( "padding", "1em" )
        , ( "box-shadow", "rgba(0, 0, 0, 0.5) 2px 2px 8px 1px" )
        ]
    ]
    [ svg -- shadow adds 9px on top and left, 13px on bottom and right
        [ SA.width "56"
        , SA.height "56"
        , SA.viewBox "-9 -9 56 56"
        , style
            [ ( "position", "absolute" )
            , if page.orientation == Above then
                ( "bottom", "-46px" )
              else
                ( "top", "-42px" )
            , ( "left"
              , case page.justify of
                  LeftAt -> "14px"
                  CenterAt -> "calc(50% - 26px)"
              )
            , ( "pointer-events", "none" )
            ]
        ]
        [ filter
            [ SA.id "aboveBlur"
            , SA.x "-40%"
            , SA.y "0"
            , SA.width "180%"
            , SA.height "140%"
            ]
            [ feGaussianBlur
                [ SA.in_ "SourceGraphic"
                , SA.stdDeviation "5"
                ]
                []
            ]
        , filter
            [ SA.id "belowBlur"
            , SA.x "-40%"
            , SA.y "-40%"
            , SA.width "180%"
            , SA.height "140%"
            ]
            [ feGaussianBlur
                [ SA.in_ "SourceGraphic"
                , SA.stdDeviation "5"
                ]
                []
            ]
        , path
            [ SA.fill "black"
            , SA.opacity "0.5"
            , if page.orientation == Above then
                SA.filter "url(#aboveBlur)"
              else
                SA.filter "url(#belowBlur)"
            , if page.orientation == Above then
                SA.d (pointer 2 2 34 page.justify page.orientation)
              else
                SA.d (pointer 2 2 30 page.justify page.orientation)
            ]
            []
        , path
            [ SA.fill "white"
            , SA.d (pointer 0 0 33 page.justify page.orientation)
            ]
            []
        , path
            [ SA.stroke "black"
            , SA.fill "none"
            , SA.d (pointer 0 0 32 page.justify page.orientation)
            ]
            []
        ]
    , span
        [ id ("tourArea" ++ toString (pageIndex + 1))
        , style
            [ ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "top"
              , if page.orientation == Below then
                  "calc(-32px + " ++ toString -page.extension ++ "em)"
                else
                  "0em"
              )
            , ( "left", "0em" )
            , ( "bottom"
              , if page.orientation == Above then
                  "calc(-32px + " ++ toString -page.extension ++ "em)"
                else
                  "0em"
              )
            , ( "right", "0em" )
            ]
        ]
        []
    , div
        []
        [ span
            [ style
                [ ( "font-size", "125%" )
                , ( "font-weight", "bold" )
                ]
            ]
            [ text (page.title ++ "\xA0")
            ]
        , text
            ( String.concat
                [ "(Page "
                , toString (pageIndex + 1)
                , "/"
                , toString pageCount
                , ")"
                ]
            )
        ]
    , button
        [ class "close"
        , Attributes.title "Close"
        , style
            [ ( "position", "absolute" )
            , ( "top", "6px" )
            , ( "right", "6px" )
            , ( "border", "none" )
            , ( "border-radius", "50%" )
            , ( "padding", "13px" )
            , ( "cursor", "pointer" )
            ]
        , onClick (Tour False (pageIndex + 1))
        ]
        [ svg
            [ SA.width "15"
            , SA.height "15"
            , SA.viewBox "-0.1 -0.1 1.2 1.2"
            , style
                [ ( "display", "block" )
                ]
            ]
            [ path
                [ SA.stroke "black"
                , SA.strokeWidth "0.2"
                , SA.d "M0,0 L1,1 M0,1 L1,0"
                ]
                []
            ]
        ]
    , div
        []
        page.paragraphs
    , div
        [ style
            [ ( "text-align", "right" )
            ]
        ]
        [ button
            [ onClick (Tour True pageIndex)
            , disabled (pageIndex == 0)
            ]
            [ text "Back"
            ]
        , text " "
        , if pageIndex + 1 == pageCount then
            button
              [ id "tourNext"
              , onClick (Tour False 1)
              ]
              [ text "Finish"
              ]
          else
            button
              [ id "tourNext"
              , onClick (Tour True (pageIndex + 2))
              ]
              [ text "Next"
              ]
        ]
    ]

pointer : Float -> Float -> Float -> Justify -> Orientation -> String
pointer dx dy height justify orientation =
  case ( justify, orientation ) of
    ( CenterAt, Above ) ->
      String.join
        " "
        [ Path.bigM (17 + dx - 0.5 * height) (33.5 + dy - height)
        , Path.l (0.5 * height) height
        , Path.l (0.5 * height) -height
        ]
    ( CenterAt, Below ) ->
      String.join
        " "
          [ Path.bigM (17 + dx - 0.5 * height) (0.5 + dy + height)
          , Path.l (0.5 * height) -height
          , Path.l (0.5 * height) height
          ]
    ( LeftAt, Above ) ->
      String.join
        " "
        [ Path.bigM (0.5 + dx) (33.5 + dy - height)
        , Path.v height
        , Path.l height -height
        ]
    ( LeftAt, Below ) ->
      String.join
        " "
        [ Path.bigM (0.5 + dx) (0.5 + dy + height)
        , Path.v -height
        , Path.l height height
        ]

type Orientation = Above | Below
type Anchor = TopOf | BottomOf
type Justify = LeftAt | CenterAt
type ExtendDummy = ExtendBy

type alias Page =
  { y : Float
  , orientation : Orientation
  , anchor : Anchor
  , gridArea : String
  , justify : Justify
  , x : Float
  , extendDummy : ExtendDummy
  , extension : Float
  , title : String
  , paragraphs : List (Html Tour)
  }

macDependentPage : Bool -> Page
macDependentPage mac =
  Page
    -0.4 Below BottomOf "lowest" LeftAt 1.9 ExtendBy 5.4
    "Text-based parameters"
    [ p
        []
        [ em [] [ text "Tempo" ]
        , text ", "
        , em [] [ text "Key" ]
        , text ", and "
        , em [] [ text "Lowest scale degree" ]
        , text " are stored as "
        , code [] [ text "key: value" ]
        , text " pairs in the textbox. The textbox is the single source of truth for these parameters; the controls just make it easier to adjust them."
        ]
    , p
        []
        [ text
            ( String.concat
                [ "Changing the same parameter any number of times will result in a single text edit that can be undone by clicking in the textbox and pressing "
                , if mac then
                    "⌘"
                  else
                    "Ctrl+"
                , "Z."
                ]
            )
        ]
    ]

playStyleHeight : Float
playStyleHeight = 4.8

pages : List Page
pages =
  [ Page
      -0.9 Below BottomOf "brand" CenterAt 18 ExtendBy 1.9
      "Basics"
      [ p
          []
          [ text "A chord is a group of notes played together. A chord progression is a sequence of chords. This website helps you discover chords and write chord progressions."
          ]
      , p
          []
          [ text "You can close this tour window at any time and then click "
          , em [] [ text "Resume tour" ]
          , text " at the top of the page to continue where you left off."
          ]
      ]
  , Page
      -2.5 Below BottomOf "keyboard" CenterAt 16 ExtendBy (15 + playStyleHeight)
      "Discovering chords"
      [ p
          []
          [ text "Select piano keys to create a custom chord. Then drag across the strings to play the notes together."
          ]
      ]
  , Page
      5.6 Below TopOf "pane" LeftAt 6 ExtendBy (7.5 + playStyleHeight)
      "Search results"
      [ p
          []
          [ text "Chords similar to your custom chord will appear in the "
          , em [] [ text "Search results" ]
          , text " view. You can click these chords to play them."
          ]
      , p
          []
          [ em [] [ text "Show custom chord" ]
          , text " will take you back to your original chord."
          ]
      ]
  , Page
      -0.4 Below BottomOf "keyboard" LeftAt 3 ExtendBy (1.5 + playStyleHeight)
      "Adding chords"
      [ p
          []
          [ text "This textbox shows the most recently played chord. Click "
          , em [] [ text "Add" ]
          , text " to add it to your chord progression."
          ]
      , p
          []
          [ text "Unknown chords are displayed as a series of numbers and cannot be added to the progression."
          ]
      ]
  , Page
      -1 Below BottomOf "song" LeftAt 6 ExtendBy (9 + playStyleHeight)
      "Chord progression"
      [ p
          []
          [ text "The chords you add will be displayed here. To hear your progression, just click each chord in left-to-right order."
          ]
      ]
  , Page
      -0.5 Below BottomOf "playStyle" LeftAt 1 ExtendBy (playStyleHeight - 0.5)
      "Play styles"
      [ p
          []
          [ text "This bar controls how chords are played when you click them."
          ]
      , p
          []
          [ text "In "
          , em [] [ text "Pad" ]
          , text " mode, chords play continuously until you click them again to stop."
          ]
      , p
          []
          [ text "In the "
          , em [] [ text "Arpeggio" ]
          , text " and "
          , em [] [ text "Strum pattern" ]
          , text " modes, you can queue up a second chord while the first one is still playing so the chord change lines up with the beat."
          ]
      ]
  , Page
      -1 Below BottomOf "theater" LeftAt 2 ExtendBy 8.9
      "Editing"
      [ p
          []
          [ text "In spite of the colorful highlighting, this is just a regular textbox. Your chord progression is stored here in text format, allowing you to edit it directly."
          ]
      , p
          []
          [ text "In addition to chord names, you can type"
          ]
      , ul
          []
          [ li
              []
              [ text "an underscore "
              , code [] [ text "_" ]
              , text " to create a space or a blank line."
              ]
          , li
              []
              [ text "two slashes "
              , code [] [ text "//" ]
              , text " to begin a comment."
              ]
          ]
      ]
  , Page
      -0.4 Below BottomOf "buffet" CenterAt 17.8 ExtendBy 7.5
      "Replacements"
      [ p
          []
          [ text "Each chord has only one accepted spelling. If you use a lowercase letter or an enharmonic equivalent like "
          , code [] [ text "A#" ]
          , text " instead of "
          , code [] [ text "Bb" ]
          , text ", this button will appear to automatically replace it with the accepted spelling."
          ]
      ]
  , Page
      -0.4 Below BottomOf "title" LeftAt 3 ExtendBy 1.5
      "Saving"
      [ p
          []
          [ em [] [ text "Save in URL" ]
          , text " puts the entire text of your chord progression in the address bar as a URL parameter. After clicking "
          , em [] [ text "Save in URL" ]
          , text ", you can add the page to your bookmarks, send the link to someone, or just close the page and find it later in your browsing history."
          ]
      , p
          []
          [ text "By default, the page title will contain the first few chords of the progression. You can enter a custom title here."
          ]
      ]
  , Page
      -0.4 Below BottomOf "scale" LeftAt 2.5 ExtendBy 1.6
      "Key"
      [ p
          []
          [ text "Chords are colored based on their harmonic function in the current key:"
          ]
      , ul
          [ style
              [ ( "list-style-type", "none" )
              , ( "padding-left", "30px" )
              ]
          ]
          [ li
              []
              [ mark
                  [ style
                      [ ( "background", "#d0a0ff" )
                      , ( "border-top-left-radius", "3px" )
                      , ( "border-bottom-left-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#b7caff" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#a2e1ff" )
                      , ( "border-top-right-radius", "3px" )
                      , ( "border-bottom-right-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , text " = tonic"
              ]
          , li
              []
              [ mark
                  [ style
                      [ ( "background", "#9effd3" )
                      , ( "border-top-left-radius", "3px" )
                      , ( "border-bottom-left-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#bdff8e" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#d6f446" )
                      , ( "border-top-right-radius", "3px" )
                      , ( "border-bottom-right-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , text " = subdominant"
              ]
          , li
              []
              [ mark
                  [ style
                      [ ( "background", "#ffad4c" )
                      , ( "border-top-left-radius", "3px" )
                      , ( "border-bottom-left-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#ff997f" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , mark
                  [ style
                      [ ( "background", "#ff7da5" )
                      , ( "border-top-right-radius", "3px" )
                      , ( "border-bottom-right-radius", "3px" )
                      ]
                  ]
                  [ text "\xA0\xA0\xA0"
                  ]
              , text " = dominant"
              ]
          , li
              []
              [ mark
                  [ style
                      [ ( "background", "#784c00" )
                      , ( "color", "white" )
                      , ( "border-radius", "3px" )
                      ]
                  ]
                  [ text "Dark background"
                  ]
              , text " = contains a diminished triad"
              ]
          ]
      , p
          []
          [ text "Choosing a key from this dropdown menu will transpose all chords in the progression while keeping their colors the same. To change key without transposing, add a line like "
          , code [] [ text "key: G" ]
          , text " or "
          , code [] [ text "key: C#m" ]
          , text " before your progression."
          ]
      ]
  , macDependentPage False
  , Page
      -0.6 Above TopOf "pane" LeftAt 1 ExtendBy 5.5
      "Settings"
      [ p
          []
          [ text "By default, this website doesn't store any data. If you want it to remember things like volume level and playback mode, go to the "
          , em [] [ text "Settings" ]
          , text " view and check "
          , em [] [ text "Remember my settings" ]
          , text "."
          ]
      ]
  ]

viewPageOptions : Int -> List (Html msg)
viewPageOptions pageNumber =
  List.indexedMap (viewPageOption pageNumber) pages

viewPageOption : Int -> Int -> Page -> Html msg
viewPageOption pageNumber pageIndex page =
  option
    [ value (toString (pageIndex + 1))
    , selected (pageIndex + 1 == pageNumber)
    ]
    [ text
        ( String.concat
            [ toString (pageIndex + 1)
            , ". "
            , page.title
            ]
        )
    ]

scrollIntoView : Int -> Cmd msg
scrollIntoView pageNumber =
  Ports.scrollIntoView ("tourArea" ++ toString pageNumber)
