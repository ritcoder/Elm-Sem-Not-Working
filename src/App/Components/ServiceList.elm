module App.Components.ServiceList exposing
  (..)


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeX exposing (decode)
import Task exposing (Task)

import App.Api as Api
import App.Context as AppContext
import Notification exposing (..)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : AppContext.Model
  , data : List ServiceInfo
  }


type alias ServiceInfo =
  { name : String
  , code : String
  , description : String
  , status : String
  , runnable : Bool
  }


serviceDecoder : Decoder ServiceInfo
serviceDecoder =
  decode ServiceInfo
  |> DecodeX.required "name" Decode.string
  |> DecodeX.required "code" Decode.string
  |> DecodeX.required "description" Decode.string
  |> DecodeX.required "status" Decode.string
  |> DecodeX.required "runnable" Decode.bool


servicesDecoder : Decoder (List ServiceInfo)
servicesDecoder =
  Decode.list serviceDecoder


init : AppContext.Model -> (Model, Cmd Msg)
init ctx =
  { context = ctx
  , data = []
  }
  |> flip (,) (toCmd2 Refresh)


name : String
name =
  "Service List"


-- UPDATE
type Msg
  = Refresh
  | NewData (Result String (List ServiceInfo))
  | NewContext AppContext.Model
  | StartService String
  | Alert String String
  
  | Close
  | Idle
  | Busy String
  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of    
    Refresh ->
      refresh model
    
    NewData result ->
      newData result model
    
    NewContext context ->
      newContext context model
    
    StartService code ->
      startService code model
    
    Alert title msg ->
      (model, info msg title)
    
    Close -> model |> noFx
    Idle -> model |> noFx
    Busy _ -> model |> noFx


refresh : Model -> (Model, Cmd Msg)
refresh model =
  let
    cmd =
      Api.get (Just model.context) "Services/getServices" servicesDecoder
      |> toCmd NewData
  in
    ( model, Cmd.batch [ cmd, toCmd2 <| Busy "Please wait..." ])


newData : Result String (List ServiceInfo) -> Model -> (Model, Cmd Msg)
newData data model =
  case data of
    Err msg ->
      ( model, Cmd.batch [ error msg name, toCmd2 Idle] )
    
    Ok d ->
      ( { model | data = d }
      ,  Cmd.batch [ toCmd2 Idle]
      )


newContext : AppContext.Model -> Model -> (Model, Cmd Msg)
newContext context model =
  let
    model' =
      { model | context = context }
  in
    ( model', Cmd.none)


startService : String -> Model -> (Model, Cmd Msg)
startService code model =
  let
    cmd =
      Api.post (Just model.context) ("Services/StartService/" ++ code) Api.emptyBody Api.emptyResponse
      |> toCmd (always <| Alert "Start Service" "Service started successfully")
  in
    ( model, cmd )


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view context model =
  div []
    [ div [ class "ui icon button", onClick Refresh ]
        [ i [ class " refresh icon" ] [] ]
    , div [ class "ui divider" ] []
    , div [ class "ui middle aligned divided animated list" ] 
        ( model.data
          |> List.map (renderServiceItem)
        )
    ]


renderServiceItem : ServiceInfo -> Html Msg
renderServiceItem item =
  div [ class "item" ]
    [ div [ class "right floated content", showButtons item ]
        [ div 
            [ class ("ui teal button " ++ if item.runnable then "" else "disabled")
            , onClick (StartService item.code)
            ] 
            [ i [ class "send icon" ] [] ]
        ]
    , i [ class "right triangle icon" ] []
    , div [ class "content" ]
        [ div [ class "header" ] [ text item.name ]
        , p [] [text item.status]
        ]
    ]


showButtons : ServiceInfo -> Attribute Msg
showButtons item =
  if item.runnable then
    style []
  else
    style [ ("display", "none")]


-- COMPONENT
type alias Container c =
  { c | serviceList : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .serviceList (\x m -> { m | serviceList = x})


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
  { icon = "random", title = "Service List", subTitle = "", width = 450, height = 0 }