module App.Components.AppPanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewContext, NewSize, NewWindow)
  )


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Cai.Common exposing (..)
import Cai.Components.ContentPanePanel as ContentPanePanel
import Cai.Components.TopPanel as TopPanel
import Cai.Components.SideBarPanel as SideBarPanel

import App.Assets as Assets
import App.Context as AppContext
import App.ComponentList exposing (..)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : Maybe AppContext.Model
  , topPanel : Maybe TopPanel.Model
  , sideBarPanel : Maybe SideBarPanel.Model
  , contentPanePanel : Maybe ContentPanePanel.Model
  }


init : Maybe AppContext.Model -> Model
init context =
  Model context Nothing Nothing Nothing


-- UPDATE
type Msg
  = NewContext AppContext.Model
  | NewSize (Int,Int)
  | PartMsg (Parts.Msg Model Msg)
  | NewWindow (Maybe WindowConfig, ComponentConfig)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewContext ctx ->
      newContext ctx model
    
    NewSize (w,h) ->
      newSize w (h - 70 - 22) model
    
    PartMsg msg' ->
      Parts.update PartMsg msg' model
    
    NewWindow (winConfig, componentConfig) ->
      newWindow winConfig componentConfig model


newWindow : Maybe WindowConfig -> ComponentConfig -> Model -> (Model, Cmd Msg)
newWindow winConfig componentConfig model =
  model
  |> updatePart2 contentPane.update (ContentPanePanel.NewWindow (winConfig, componentConfig))


newContext context model =
  { model | context = Just context }
  |> updatePart2 topPanel.update (TopPanel.NewContext context)
  |> updatePart contentPane.update (ContentPanePanel.NewContext context)
  |> updatePart sideBarPanel.update (SideBarPanel.NewContext context)


newSize w h model =
  model
  |> updatePart2 contentPane.update (ContentPanePanel.NewSize (w,h))


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div [ wrapperStyle ] 
    [ topPanel.view model
    , sideBarPanel.view ctx model
    , contentPane.view model
    ]


topPanel : TopPanel.Part Model Msg
topPanel =
  TopPanel.part PartMsg (TopPanel.init Nothing) []


sideBarPanel : SideBarPanel.Part Model Msg
sideBarPanel =
  SideBarPanel.part PartMsg (SideBarPanel.init Nothing) []


contentPane : ContentPanePanel.Part Model Msg
contentPane =
  ContentPanePanel.part PartMsg (ContentPanePanel.init Nothing) []


wrapperStyle : Attribute Msg
wrapperStyle =
  style
    [ ("height", "100%")
    , ("width", "100%")
    ]


-- COMPONENT
type alias Container c =
  { c | appPanel : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .appPanel (\x m -> { m | appPanel = x})