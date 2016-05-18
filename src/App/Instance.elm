module App.Instance exposing (..)


import Html exposing (..)
import Task exposing (Task)
import Window exposing (Size)

import App.AllPorts exposing (..) 
-- (clearUser, createWindow, suggestions)
import App.Context as AppContext exposing (User, AppInfo)
import App.Components.AppPanel as AppPanel
import App.Components.Authentication.LoginPanel as LoginPanel
import App.ComponentList exposing (ComponentConfig, WindowConfig)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { loginPanel : Maybe LoginPanel.Model
  , appPanel : Maybe AppPanel.Model
  , context : AppContext.Model
  }


init : AppContext.Model -> (Model, Cmd Msg)
init context =
  let
    model = 
      Model Nothing Nothing context
    fx =
      Cmd.batch
        [ toCmd (\{width, height} -> NewSize (width, height)) Window.size
        , toCmd NewContext <| Task.succeed context
        ]
  in
    (model, fx)


-- UPDATE


type Msg 
  = PartMsg (Parts.Msg Model Msg)
  | NewContext AppContext.Model
  | NewSize (Int, Int)
  | NewWindow (Maybe WindowConfig, ComponentConfig)
  | Logout


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PartMsg msg' ->
      Parts.update PartMsg msg' model
    
    NewSize (w, h) ->
      newSize w h model    
    
    NewContext ctx ->
      newContext ctx model
      
    NewWindow (win, component) ->
      newWindow win component model
    
    Logout ->
      logout model


newSize : Int -> Int -> Model -> (Model, Cmd Msg)
newSize w h model =
  let
    (model', fx1) =
      appPanel.update (AppPanel.NewSize (w,h)) model
  in
    (model', fx1)


newContext : AppContext.Model -> Model -> (Model, Cmd Msg)
newContext ctx  model =
      { model 
        | context = ctx
      }
      |> updatePart2 loginPanel.update (LoginPanel.NewContext ctx)
      |> updatePart appPanel.update (AppPanel.NewContext ctx)


newWindow : Maybe WindowConfig -> ComponentConfig -> Model -> (Model, Cmd Msg)
newWindow winConfig componentConfig model =
  model
  |> updatePart2 appPanel.update (AppPanel.NewWindow (winConfig, componentConfig))


logout : Model -> (Model, Cmd Msg)
logout model =
    (model, Cmd.none) --todo:


-- VIEW

view : Model -> Html Msg
view model =
  case model.context.user of
    Just _ ->
      appPanel.view model.context model
    
    Nothing ->
      loginPanel.view model.context model


loginPanel : LoginPanel.Part Model Msg
loginPanel =
  LoginPanel.part PartMsg LoginPanel.init []


appPanel : AppPanel.Part Model Msg
appPanel =
  AppPanel.part PartMsg (AppPanel.init Nothing) []


-- Others
subscriptions : Model -> Sub Msg
subscriptions model =
  [ Window.resizes (\{width, height} -> NewSize (width, height))
  , createWindow NewWindow
  , clearUser (\x -> Logout)
  ]
  |> Sub.batch