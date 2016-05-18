module App.Components.Authentication.UserSettingsPanel exposing (..)


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

import App.Api as Api
import App.Context as AppContext
import Notification exposing (error, info)
import Parts
import Utils exposing (..)



-- MODEL
type alias Model =
  { context : AppContext.Model }


init : AppContext.Model -> (Model, Cmd Msg)
init ctx =
  Model ctx |> noFx


name : String
name =
  "User Settings"


-- UPDATE
type Msg
  = NoOp
  
  | Busy String
  | Idle
  | Close


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of    
    _ ->
      model |> noFx


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  div [ class "ui info message" ]
    [ text "There are currently no user settings" ]


-- COMPONENT
type alias Container c =
  { c | userSettings : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .userSettings (\x m -> { m | userSettings = x})


observe :
  { b | busy : String -> a, close: a, idle: a }
  -> Msg
  -> Maybe a
observe observer msg = 
  case msg of
    Idle ->
      Just observer.idle
    
    Busy msg ->
      Just <| observer.busy msg
    
    Close ->
      Just observer.close
    
    _ ->
      Nothing


partView :
  AppContext.Model
  -> (Parts.Msg (Container a) b -> b)
  -> (Parts.ContextualView AppContext.Model (Container a) b -> d)
  -> { e | busy : String -> b, close: b, idle: b }
  -> ( d, Cmd b )
partView context msgTagger toPartView observer =
  let
    (model, fx) =
      init context
    instance =
      part msgTagger model [ observe observer ]
  in 
    (toPartView instance.view,Cmd.map instance.fwd fx)


winConfig =
  { icon = "settings", title = "User Settings", subTitle = "",  width = 400, height = 0 }