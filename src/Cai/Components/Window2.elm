module Cai.Components.Window2 exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewSize, PartMsg, Set)
  , onClose
  , fromWinMsg
  )


-- IMPORTS
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Cai.Common as Common exposing (..)

import App.Context as AppContext
import App.PartStore as PartStore
import Parts exposing (Indexed)
import Utils exposing (..)


-- MODEL
-- type alias ChildRenderer = AppContext.Model -> PartStore.Model -> Html Msg

type alias Model =
  { busy : Bool
  , busyMsg : String
  , icon : String
  , title : String
  , subTitle : String
  , viewport : (Int, Int)
  , partStore : PartStore.Model
  , child : WindowChild
  , xxx : String
  -- , child2 : ChildRenderer
  }


init : WindowChild -> Model
init child =
  { busy = False 
  , busyMsg = ""
  , icon = "list layout"
  , title = "Test Window"
  , subTitle = "[sub title here]"
  , viewport = (450, 600)
  , partStore = PartStore.init
  , child = child
  , xxx = ""
  -- , child2 = view2
  }


-- ACTION, UPDATE
type Msg
  = Close
  | PartMsg (Parts.Msg PartStore.Model Msg)
  | PartMsg2 (Parts.Msg PartStore.Model WinMsg)
  | ChildMsg (PartStore.Model -> (PartStore.Model, Cmd WinMsg))
  | NewSize (Int, Int)
  | NoOp
  | Set String


fromWinMsg : WinMsg -> Msg
fromWinMsg msg =
  case msg of
    Common.Close ->
      Close
    
    Common.PartMsg msg' ->
      PartMsg2 msg'
    
    Common.ChildMsg updateFn ->
      ChildMsg updateFn
      
    _ ->
      NoOp


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PartMsg msg' ->
      partMsg msg' model
    
    PartMsg2 msg' ->
      partMsg2 msg' model
    
    ChildMsg update' ->
      let
        (ps, fx) =
          update' model.partStore
        model' =
          { model | partStore = ps}
      in
        model' |> noFx
    
    
    Set x ->
      let
        ps =
          model.partStore
        ps' =
          { ps | xxx = x}
        model' =
          { model | xxx = x, partStore = ps' }
      in
        model' |> noFx
    
    _ ->
      model |> noFx


partMsg2 : Parts.Msg PartStore.Model WinMsg -> Model -> (Model, Cmd Msg)
partMsg2 msg model =
  let
    (partStore', fx) =
      Parts.update Common.PartMsg msg model.partStore
    model' =
      { model | partStore = partStore' }
  in
    (model', Cmd.map fromWinMsg fx)


partMsg : Parts.Msg PartStore.Model Msg -> Model -> (Model, Cmd Msg)
partMsg msg  model =
  let
    (partStore', fx) =
      Parts.update PartMsg msg model.partStore
    model' =
      { model | partStore = partStore' }
  in
    (model', fx)


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div [ class "window", windowStyle model ]
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
        [ Html.map fromWinMsg <| model.child.view ctx model.partStore
        -- , div [ class "ui segment"]
        --     [ div [] 
        --         [ text "Test"
        --         , input [ type' "text", onInput Set ] []
        --         , text <| toString model.xxx
        --         ]
        --     ]
        -- , model.child2 ctx model.partStore
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