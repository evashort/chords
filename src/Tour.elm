module Tour exposing
  ( shadowBuffet, padSong, paneShadow
  , Tour, init, view, viewPageOptions, scrollIntoView
  )

import Buffet exposing (Buffet)
import Pane exposing (Pane)
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
  if tour.visible && tour.pageNumber == 2 then
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
        case tour.pageNumber of
          3 -> 1
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
    case tour.pageNumber of
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
    if mac && tour.pageNumber == macPageNumber then
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
        , if page.above then
            ( "bottom"
            , "calc(100% + 32px + " ++ toString page.distance ++ "em)"
            )
          else
            ( "top"
            , "calc(100% + 32px + " ++ toString page.distance ++ "em)"
            )
        , ( "left"
          , if page.center then
              toString (page.left - 15) ++ "em"
            else
              toString page.left ++ "em"
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
            , if page.above then
                ( "bottom", "-46px" )
              else
                ( "top", "-42px" )
            , ( "left"
              , if page.center then
                  "calc(50% - 26px)"
                else
                  "14px"
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
            , if page.above then
                SA.filter "url(#aboveBlur)"
              else
                SA.filter "url(#belowBlur)"
            , if page.above then
                SA.d (pointer "m2,2 " 34 page.center page.above)
              else
                SA.d (pointer "m2,2 " 30 page.center page.above)
            ]
            []
        , path
            [ SA.fill "white"
            , SA.d (pointer "" 33 page.center page.above)
            ]
            []
        , path
            [ SA.stroke "black"
            , SA.fill "none"
            , SA.d (pointer "" 32 page.center page.above)
            ]
            []
        ]
    , span
        [ id ("tourArea" ++ toString (pageIndex + 1))
        , style
            [ ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "top"
              , if not page.above then
                  toString -page.extension ++ "em"
                else
                  "0em"
              )
            , ( "left", "0em" )
            , ( "bottom"
              , if page.above then
                  toString -page.extension ++ "em"
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

pointer : String -> Int -> Bool -> Bool -> String
pointer offset height center above =
  if center then
    let
      h = toString height
    in let
      half = toString (0.5 * toFloat height)
    in
      if above then
        String.concat
          [ "M17,33.5 ", offset
          , "m-" , half , ",-" , h
          , "l" , half , "," , h
          , "l" , half , ",-" , h
          ]
      else
        String.concat
          [ "M17,0.5 ", offset
          , "m-" , half , "," , h
          , "l" , half , ",-" , h
          , "l" , half , "," , h
          ]
  else
    if above then
      String.join
        (toString height)
        [ "M0.5,33.5 " ++ offset ++ "m0,-"
        , " v"
        , " l"
        , ",-"
        , ""
        ]
    else
      String.join
        (toString height)
        [ "M0.5,0.5 " ++ offset ++ "m0,"
        , " v-"
        , " l"
        , ","
        , ""
        ]

type alias Page =
  { center : Bool
  , above : Bool
  , distance : Float
  , left : Float
  , extension : Float
  , gridArea : String
  , title : String
  , paragraphs : List (Html Tour)
  }

macDependentPage : Bool -> Page
macDependentPage mac =
  Page
    False False -0.4 1.9 7.3
    "lowest"
    "Text-based parameters"
    [ p
        []
        [ text "All settings above the textbox are stored as "
        , code [] [ text "key: value" ]
        , text " pairs in the textbox itself. The textbox is the single source of truth for these parameters; the controls just make it easier to adjust them."
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

pages : List Page
pages =
  [ Page
      False False -1 2 7
      "theater"
      "Textbox"
      [ p
          []
          [ text "This is the textbox for writing chord progressions. For example,"
          ]
      , ul
          []
          [ li
              []
              [ text "B♭"
              , sup [] [ text "sus4" ]
              , text " is written as "
              , code [] [ text "Bbsus4" ]
              , text "."
              ]
          , li
              []
              [ text "Diminished chords like B"
              , sup [] [ text "o" ]
              , text " are written as "
              , code [] [ text "Bo" ]
              , text " with a lowercase o."
              ]
          , li
              []
              [ text "Half-diminished chords like B"
              , sup [] [ text "ø" ]
              , text " (also known as B"
              , sup [] [ text "7♭5" ]
              , text ") are written as "
              , code [] [ text "B0" ]
              , text " with a zero."
              ]
          , li
              []
              [ text "An underscore "
              , code [] [ text "_" ]
              , text " creates a space or a blank line."
              ]
          , li
              []
              [ text "Two slashes "
              , code [] [ text "//" ]
              , text " begin a comment."
              ]
          ]
      , p
          []
          [ text "Chords are highlighted based on their harmonic function:"
          ]
      , ul
          [ style
              [ ( "list-style-type", "none" )
              , ( "padding-left", "30px" )
              ]
          ]
          [ li
              []
              [ span
                  [ class "colorLegend"
                  , style
                      [ ( "background", "linear-gradient(to right, #d0a0ff, #b7caff, #a2e1ff)" )
                      ]
                  ]
                  []
              , text " = tonic"
              ]
          , li
              []
              [ span
                  [ class "colorLegend"
                  , style
                      [ ( "background", "linear-gradient(to right, #9effd3, #bdff8e, #d6f446)" )
                      ]
                  ]
                  []
              , text " = subdominant"
              ]
          , li
              []
              [ span
                  [ class "colorLegend"
                  , style
                      [ ( "background", "linear-gradient(to right, #ffad4c, #ff997f, #ff7da5)" )
                      ]
                  ]
                  []
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
          [ text "You can visit the \"View on Github\" link for more information about the colors."
          ]
      ]
  , Page
      True False -0.4 17.8 9.5
      "buffet"
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
      False False -1 6 6
      "song"
      "Play buttons"
      [ p
          []
          [ text "Instead of a single button that plays the entire progression, there is a separate play button for each chord you type. To hear the progression, you have to click each button in order."
          ]
      , p
          []
          [ text "There are several playback modes. In the arpeggio and strum pattern modes, you can queue up a second chord while the first one is still playing so the chord change lines up with the beat."
          ]
      ]
  , Page
      False False -0.4 3 3.5
      "title"
      "Saving your work"
      [ p
          []
          [ text "The \"Save in URL\" button puts the entire text of your chord progression in the address bar as a URL parameter. After clicking \"Save in URL\", you can add the page to your bookmarks, send the link to someone, or just close the page and find it later in your browsing history."
          ]
      , p
          []
          [ text "By default, the page title will contain the first several chords in the textbox. You can enter a custom title here."
          ]
      ]
  , macDependentPage False
  , Page
      False False -0.4 2.5 3.6
      "scale"
      "Key"
      [ p
          []
          [ text "Choosing a key from this dropdown menu will transpose all chords in the textbox. It will also set the "
          , code [] [ text "key" ]
          , text " parameter, which mainly affects how the chords are colored."
          ]
      , p
          []
          [ text "If you wanted to switch to G Major without transposing, you could type "
          , code [] [ text "key: G" ]
          , text " at the start of the textbox."
          ]
      ]
  , Page
      True False -0.6 15.5 3.3
      "lowest"
      "Lowest scale degree"
      [ p
          []
          [ text "This slider adjusts the range of the piano keyboard. Moving it up will transpose the lowest chords up one octave."
          ]
      , p
          []
          [ text "In the textbox, this parameter is stored as a number which always represents a "
          , em [] [ text "major" ]
          , text " scale degree."
          ]
      ]
  , Page
      False True -0.6 1 7.5
      "pane"
      "Settings"
      [ p
          []
          [ text "You'll probably want to check \"Remember my settings\" so you don't have to switch to your preferred playback mode every time you visit the site."
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
