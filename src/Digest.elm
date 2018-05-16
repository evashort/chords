module Digest exposing
  ( Digest, fromString, update, undo, redo, hardReplace, view
  , beforeAction, replace
  )

import Replacement exposing (Replacement)
import UndoCatcher exposing (UndoCatcher)

import Html exposing (Html)

type alias Digest =
  { catcher : UndoCatcher
  , history : List State
  , future : List State
  }

type alias State =
  { text : String
  , nextAction : String
  }

fromString : String -> Digest
fromString text =
  { catcher = UndoCatcher.fromString text
  , history = []
  , future = []
  }

update : String -> Digest -> Digest
update text digest =
  { catcher = UndoCatcher.update text digest.catcher
  , history = []
  , future = []
  }

undo : Digest -> Digest
undo digest =
  { catcher = UndoCatcher.undo digest.catcher
  , history = List.drop 1 digest.history
  , future = List.take 1 digest.history ++ digest.future
  }

redo : Digest -> Digest
redo digest =
  { catcher = UndoCatcher.redo digest.catcher
  , history = List.take 1 digest.future ++ digest.history
  , future = List.drop 1 digest.future
  }

hardReplace : Replacement -> Digest -> Digest
hardReplace replacement digest =
  { catcher = UndoCatcher.replace replacement digest.catcher
  , history = []
  , future = []
  }

view : Digest -> Html String
view digest =
  UndoCatcher.view digest.catcher

beforeAction : String -> Digest -> String
beforeAction action digest =
  case digest.history of
    [] ->
      digest.catcher.frame.text
    state :: _ ->
      if state.nextAction == action then
        state.text
      else
        digest.catcher.frame.text

replace : String -> Maybe Replacement -> Digest -> Digest
replace action mr digest =
  case ( latestActionIs action digest.history, mr ) of
    ( True, Nothing ) ->
      { catcher = UndoCatcher.hardUndo digest.catcher
      , history = List.drop 1 digest.history
      , future = []
      }
    ( True, Just replacement ) ->
      { catcher = UndoCatcher.switch replacement digest.catcher
      , history = digest.history
      , future = []
      }
    ( False, Nothing ) ->
      digest
    ( False, Just replacement ) ->
      let
        state =
          { text = digest.catcher.frame.text
          , nextAction = action
          }
      in
        { catcher = UndoCatcher.replace replacement digest.catcher
        , history = state :: digest.history
        , future = []
        }

latestActionIs : String -> List State -> Bool
latestActionIs action states =
  case states of
    [] ->
      False
    state :: _ ->
      state.nextAction == action
