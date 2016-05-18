module Cai.Common exposing (..)


-- IMPORTS
import Html exposing (..)

import App.Context as AppContext
-- import App.Messaging
import App.PartStore as PartStore
import Parts


-- window related stuff
-- type alias WindowConfig =
--   { icon : String
--   , title : String
--   , subTitle : String
--   , width : Int
--   , height : Int
--   }


-- type alias WindowChild =
--   { view : AppContext.Model -> PartStore.Model -> Html WinMsg
--   -- , update : WinMsg -> PartStore.Model -> (PartStore.Model, Cmd WinMsg)
--   }


type alias PartView =
  AppContext.Model -> PartStore.Model -> Html WinMsg


-- toPartView : (ctx -> model -> Html msg} -> ctx -> model -> Html msg
-- toPartView : Parts.ContextualView AppContext.Model (container) b -> d
toPartView view =
  \ctx m ->
    div [] [ view ctx m ]


type WinMsg
  = Close
  | Idle
  | Busy String
  | PartMsg (Parts.Msg PartStore.Model WinMsg)
  | NewSize (Int, Int)
  | ChildMsg (PartStore.Model -> (PartStore.Model, Cmd WinMsg))