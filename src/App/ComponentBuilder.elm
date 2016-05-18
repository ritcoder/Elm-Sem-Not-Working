module App.ComponentBuilder exposing 
  (buildWindow)

import App.ComponentList exposing (..)
import App.Context as AppContext

import App.Components.Firm as Firm
import App.Components.ServiceList as ServiceList
import App.Components.Calculator as Calculator
import App.Components.Authentication.ChangePasswordPanel as ChangePassword
import App.Components.Authentication.UserProfilePanel as UserProfile
import App.Components.Authentication.UserSettingsPanel as UserSettings

import App.Components.Lookups.IdTypesPanel as IdTypes

import Cai.Common as Common exposing (..)
import Cai.Components.Window as Window


-- buildWindow : AppContext.Model -> Int -> Int -> Maybe WinConfig -> ComponentConfig -> 
buildWindow context id initialHeight winConfig componentConfig partMsg observations =
  let
    component =
      toComponent componentConfig
    window =
      createWindowConfig winConfig initialHeight component
  in
    createWindow2 context id window component observations partMsg  


createWindow2 context id window component observations partMsg =
  let
    windowId =
      "window-" ++ toString id
    (winModel, winFx) = 
      Window.init windowId window partView
    winPart =
      Window.part id partMsg winModel observations
    observer = 
      { busy = Common.Busy
      , idle = Common.Idle
      , close = Common.Close
      } 
    (partView, partFx) =
      getPartView context component observer
    fx =
      Cmd.batch [winFx, Cmd.map Window.fromWinMsg partFx ]
  in
    (winPart, Cmd.map winPart.fwd fx)


createWindowConfig winConfig height component =
  let
    winConfig' =
      case winConfig of
        Nothing ->
          getWindowConfig component
        
        Just x ->
          x
  in
    { winConfig' | height = height }


getPartView :
  AppContext.Model
  -> Component
  -> { b | busy : String -> WinMsg, close: WinMsg, idle: WinMsg }
  -> (PartView, Cmd WinMsg)
getPartView context component observer =
  case component of
    Firm -> Firm.partView context Common.PartMsg toPartView observer
    ServiceList -> ServiceList.partView context Common.PartMsg toPartView observer
    Calculator -> Calculator.partView context Common.PartMsg toPartView observer    
    
    UserProfile -> UserProfile.partView context Common.PartMsg toPartView observer
    UserSettings -> UserSettings.partView context Common.PartMsg toPartView observer
    ChangePassword -> ChangePassword.partView context Common.PartMsg toPartView observer
    
    IdTypes -> IdTypes.partView context Common.PartMsg toPartView observer


getWindowConfig component = 
  case component of
    ChangePassword -> ChangePassword.winConfig    
    UserProfile -> UserProfile.winConfig
    UserSettings -> UserSettings.winConfig
    IdTypes -> IdTypes.winConfig
    ServiceList -> ServiceList.winConfig
    
    _ ->
      { icon = "block layout"
      , title = toString component
      , subTitle = ""
      , width = 450
      , height = 0
      }