port module UndoCatcher exposing
  ( undoPort, redoPort, UndoCatcher, fromString, update, undo, redo
  , replace, switch, view
  )

import Html exposing (Html, div, textarea)
import Html.Attributes exposing (style, id, value, property, attribute)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed
import Json.Encode

port undoPort : (() -> msg) -> Sub msg

port redoPort : (() -> msg) -> Sub msg

type alias UndoCatcher =
  { frame : Frame
  , edits : List Edit
  , editCount : Int
  , futureEdits : List Edit
  }

type alias Edit =
  { before : Frame
  , after : Frame
  }

type alias Frame =
  { text : String
  , start : Int
  , stop : Int
  }

fromString : String -> UndoCatcher
fromString text =
  { frame =
      { text = text
      , start = String.length text
      , stop = String.length text
      }
  , edits = []
  , editCount = 0
  , futureEdits = []
  }

update : String -> UndoCatcher -> UndoCatcher
update text catcher =
  let frame = catcher.frame in
    { catcher | frame = { frame | text = text } }

undo : UndoCatcher -> UndoCatcher
undo catcher =
  case catcher.edits of
    [] ->
      catcher
    edit :: edits ->
      { catcher
      | frame = edit.before
      , edits = edits
      , editCount = catcher.editCount - 1
      , futureEdits = edit :: catcher.futureEdits
      }

redo : UndoCatcher -> UndoCatcher
redo catcher =
  case catcher.futureEdits of
    [] ->
      catcher
    edit :: futureEdits ->
      { catcher
      | frame = edit.after
      , edits = edit :: catcher.edits
      , editCount = catcher.editCount + 1
      , futureEdits = futureEdits
      }

replace : Int -> Int -> String -> UndoCatcher -> UndoCatcher
replace start stop replacement catcher =
  let
    after =
      { text =
          String.concat
            [ String.left start catcher.frame.text
            , replacement
            , String.dropLeft stop catcher.frame.text
            ]
      , start = start + String.length replacement
      , stop = start + String.length replacement
      }
  in
    { catcher
    | frame = after
    , edits =
        { before =
            { text = catcher.frame.text
            , start = start
            , stop = stop
            }
        , after = after
        } ::
          catcher.edits
    , editCount = catcher.editCount + 1
    , futureEdits = []
    }

switch : Int -> Int -> String -> UndoCatcher -> UndoCatcher
switch start stop replacement catcher =
  case catcher.edits of
    [] ->
      catcher
    oldEdit :: rest ->
      let
        after =
          { text =
              String.concat
                [ String.left start oldEdit.before.text
                , replacement
                , String.dropLeft stop oldEdit.before.text
                ]
          , start = start + String.length replacement
          , stop = start + String.length replacement
          }
      in
        { catcher
        | frame = after
        , edits = { oldEdit | after = after } :: rest
        , futureEdits = []
        }

view : UndoCatcher -> Html String
view catcher =
  Keyed.node
    "div"
    [ style
        [ ( "position", "relative" )
        , ( "height", "100%" )
        ]
    ]
    ( List.concat
        [ List.map2
            (viewHiddenFrame cancelUndo)
            ( List.range
                (catcher.editCount - List.length catcher.edits)
                (catcher.editCount - 1)
            )
            (List.map .before (List.reverse catcher.edits))
        , [ viewFrame catcher ]
        , List.map2
            (viewHiddenFrame cancelRedo)
            ( List.range
                (catcher.editCount + 1)
                (catcher.editCount + List.length catcher.futureEdits)
            )
            (List.map .after catcher.futureEdits)
        ]
    )

viewFrame : UndoCatcher -> (String, Html String)
viewFrame catcher =
  ( toString catcher.editCount
  , textarea
      [ onInput identity
      , attribute "onkeydown" "lockCatcher(event)"
      , attribute "oninput" "unlockCatcher(event)"
      , id "catcher"
      , value catcher.frame.text
      , property
          "selectionStart"
          (Json.Encode.int catcher.frame.start)
      , property
          "selectionEnd"
          (Json.Encode.int catcher.frame.stop)
      , property "lockedValue" Json.Encode.null
      , property
          "undoValue"
          ( case catcher.edits of
              [] -> Json.Encode.null
              edit :: _ -> (Json.Encode.string edit.after.text)
          )
      , property
          "redoValue"
          ( case catcher.futureEdits of
              [] -> Json.Encode.null
              edit :: _ -> (Json.Encode.string edit.before.text)
          )
      , style
          [ ( "position", "absolute" )
          , ( "top", "0" )
          , ( "left", "0" )
          , ( "width", "100%" )
          , ( "height", "100%" )
          , ( "box-sizing", "border-box" )
          , ( "padding", "10px" )
          , ( "border-width", "2px" )
          , ( "margin", "0" )
          , ( "font", "inherit" )
          , ( "background", "transparent" )
          ]
      ]
      []
  )

viewHiddenFrame : String -> Int -> Frame -> (String, Html msg)
viewHiddenFrame inputScript i frame =
  ( toString i
  , textarea
      [ value frame.text
      , attribute "oninput" inputScript
      , property "lockedValue" (Json.Encode.string frame.text)
      , style
          [ ( "visibility", "hidden" )
          , ( "position", "absolute" )
          , ( "left", "0" )
          , ( "bottom", "0" )
          , ( "width", "100%" )
          , ( "height", "30%" )
          , ( "box-sizing", "border-box" )
          , ( "padding", "10px" )
          , ( "border-width", "2px" )
          , ( "margin", "0" )
          , ( "font", "inherit" )
          ]
      ]
      []
  )

cancelUndo : String
cancelUndo =
  "if (event.target.value != event.target.lockedValue) document.execCommand(\"redo\", true, null)"

cancelRedo : String
cancelRedo =
  "if (event.target.value != event.target.lockedValue) document.execCommand(\"undo\", true, null)"
