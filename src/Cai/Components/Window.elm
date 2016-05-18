module Cai.Components.Window exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewSize, PartMsg)
  , onClose
  , fromWinMsg
  )


-- IMPORTS
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Cai.Common as Common exposing (..)
import App.ComponentList exposing (..)
import App.Context as AppContext
import App.PartStore as PartStore
import Parts exposing (Indexed)
import Utils exposing (..)


type alias Model =
  { busy : Bool
  , busyMsg : String
  , icon : String
  , title : String
  , subTitle : String
  , viewport : (Int, Int)
  , partStore : PartStore.Model
  , child : PartView
  , id : String
  }


init : String -> WindowConfig -> PartView -> (Model, Cmd Msg)
init id config part =
  { busy = False
  , busyMsg = ""
  , icon = config.icon
  , title = config.title
  , subTitle = config.subTitle
  , viewport = 
      ( config.width
      , case config.height of 
          0 -> 300
          _ -> config.height
      )
  , partStore = PartStore.init
  , child = part
  , id = id
  }
  |> flip (,) Cmd.none


-- UPDATE
type Msg
  = Close
  | Busy String
  | Idle
  | PartMsg (Parts.Msg PartStore.Model WinMsg)
  | NewSize (Int, Int)
  | NoOp


fromWinMsg : WinMsg -> Msg
fromWinMsg msg =
  case msg of
    Common.Close ->
      Close
    
    Common.PartMsg msg' ->
      PartMsg msg'
    
    Common.Busy text ->
      Busy text
    
    Common.Idle ->
      Idle
    
    _ ->
      NoOp


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PartMsg msg' ->
      partMsg msg' model
    
    NewSize (w, h) ->
      newSize w h model
    
    Close ->
      model |> noFx
    
    NoOp ->
      model |> noFx
    
    Busy busyMsg ->
      { model
        | busy = True
        , busyMsg = busyMsg
      } |> noFx
    
    Idle ->
      { model 
        | busy = False
        , busyMsg = ""
      } |> noFx


partMsg : Parts.Msg PartStore.Model WinMsg -> Model -> (Model, Cmd Msg)
partMsg msg model =
  let
    (partStore', fx) =
      Parts.update Common.PartMsg msg model.partStore
    model' =
      { model | partStore = partStore' }
  in
    (model', Cmd.map fromWinMsg fx)


newSize : Int -> Int -> Model -> (Model, Cmd Msg)
newSize w h model =
  let
    model' =
      { model | viewport = (fst model.viewport, h) }
  in
    ( model', Cmd.none)


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div [ class "window", windowStyle model, id model.id ]
    [ div [ class "ui text menu", style [ "margin-top" => "0" ] ]
        [ div [ class "item" ]
            [ i [ class <| "black icon " ++ model.icon ] []
            , text model.title
            ]
        , div [ class "right menu" ]
            [ div [ class "item", onClick Close ]
                [ i [ class "ui link remove icon" ] [] ]
            ]
        ]
    , div [ class "ui segment", contentStyle model ]
        [ Html.map fromWinMsg <| model.child ctx model.partStore
        , div [ classList [ ("ui inverted dimmer", True), ("active", model.busy)] ]
            [ div [ class "ui text loader"] [ text model.busyMsg] ]
        ]
    ]



(=>) = (,)


windowStyle model =
  let 
    (w,h) = model.viewport
    width = (toString w) ++ "px"
    height = (toString (h-5)) ++ "px"
  in
    style
      [ "background-color" => "darkgrey"
      , "margin" => "5px"
      , "padding" => "0px 5px 5px 5px"
      , "float" => "left"
      , "width" => width
      , "height" => height
      ]


contentStyle model =
  let 
    (w,h) = model.viewport
    height = h - 90
    heightPx = (toString height) ++ "px"
  in
    style
      [ "height" => heightPx
      , "overflow-y" => "auto"
      , "z-index" => "0"
      ]


--todo: convert to a part
-- COMPONENT
type alias Container c =
  { c | window : Indexed Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  Int
  -> (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part index =
  Parts.instanceWithContext view update .window (\x m -> { m | window = x}) index


onClose : (Msg -> a) -> Msg -> Maybe a
onClose f msg =
  case msg of
    Close ->
      Just (f msg)
    
    _ ->
      Nothing

