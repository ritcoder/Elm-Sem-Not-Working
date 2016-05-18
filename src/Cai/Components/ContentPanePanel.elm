module Cai.Components.ContentPanePanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg(NewSize, NewContext, NewWindow)
  )


-- IMPORTS
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Cai.Common as Common exposing (..)
import Cai.Components.Window as Window

import App.Components.ServiceList as Component

import App.ComponentBuilder exposing (buildWindow)
import App.ComponentList exposing (..)
import App.Context as AppContext
import Parts exposing (Indexed)
import Utils exposing (..)


-- import App.Components.Calculator as Calculator

-- MODEL
type alias Model =
  { context : Maybe AppContext.Model
  , nextId : Int
  , windows : Dict Int (Window.Part PartStore Msg)
  , wrapperHeight : Int
  , containerHeight : Int
  , windowHeight : Int
  , viewport : (Int, Int)
  , partStore : PartStore
  }


type alias PartStore =
  { window : Indexed Window.Model }


init : Maybe AppContext.Model -> Model
init context =
  let
    model =
      { context = context
      , nextId = 0
      , windows = Dict.empty
      , wrapperHeight = 0
      , containerHeight = 0
      , windowHeight = 0
      , viewport = (0,0)
      , partStore =
          { window = Dict.empty }
      }
    model' =
      model
      -- |> createCalc
      -- |> createWindow testConfig
      -- |> createWindow testConfig
  in
    model'


-- testConfig =
--   { icon = "blue user"
--   , title = "Test Window"
--   , subTitle = "..."
--   , width = 450
--   , height = 0
--   -- , child = 
--   --     { view = \ctx m -> div [] [ text "Some child here" ]
--   --     }
--   }
  
-- -- todo: rewrite and move to update

-- createWindow : WindowConfig -> Model -> Model
-- createWindow config model =
--   let
--     viewFn = 
--       let
--         part =
--           Component.part Common.PartMsg (Component.init model.context) []
--           -- Lookups.part Common.PartMsg (Lookups.init) []
--       in
--         \ctx m ->
--           div [] [ part.view ctx m ]
--     in
--       createWindow2 config viewFn model

-- createWindow2 : WindowConfig -> PartView -> Model -> Model
-- createWindow2 config viewFn model =
--   let
--     -- child2 =
--     --   { view = 
--     --       \ctx m m2 ->
--     --         div []
--     --           [ text "Other child herer"
--     --           , input [ type' "text", onInput Window.Set ] []
--     --           -- , text <| m.xxx
--     --           -- , part.view ctx m |> Html.map Window.fromWinMsg
--     --           ]
        
--     --   }
    
--     -- child =
--     --   { view = viewFn }
    
     
--     -- lastId =
--     --   model.nextId
--     -- winConfig =
--     --   Window.init config viewFn -- child2.view-- (\ctx m -> div [] [ text "Some child here" ])--child --todo: window config
--     config' =
--       { config | height = model.windowHeight }
--     winPart =
--       Window.part model.nextId PartMsg (Window.init config' viewFn) 
--         [ Window.onClose (always <| CloseWindow model.nextId) ]
--     model' =
--       { model
--         | nextId = model.nextId + 1
--         , windows = Dict.insert model.nextId winPart model.windows
--       }
--   in
--     model'

-- -- createCalc model =
-- --   let
-- --     calc =
-- --       Calculator.init
-- --     updateFn =
-- --       \msg m ->
-- --         let
-- --           inst = 
-- --             case m.calculator of
-- --               Just calc ->
-- --                 calc
-- --               Nothing ->
-- --                 Calculator.init
-- --           (inst', fx) =
-- --             Calculator.update msg inst 
-- --           -- _ =
-- --           --   msg
-- --           --   |> Calculator.update
            
-- --           --   |> Debug.log "in update fn"
-- --           -- inst' =
-- --           --   { inst | calculation = "testing", current = "192", accumulator = inst.accumulator + 5 }
-- --           m' =
-- --             { m | calculator = Just inst'}
-- --         in
-- --         (m', Cmd.none)
-- --     toWinMsg msg =
-- --       let
-- --         _ =
-- --           Debug.log "processing msg" msg
-- --       in
-- --       Common.ChildMsg <| updateFn msg
-- --     viewFn =
-- --       \ctx m ->
-- --         let
-- --           inst = 
-- --             case m.calculator of
-- --               Just calc ->
-- --                 calc
-- --               Nothing ->
-- --                 Calculator.init
-- --         in
-- --           div [] [ Calculator.view inst |> Html.map (toWinMsg) ]
-- --     winConfig =
-- --       Window.init { view = viewFn } 
-- --         -- viewFn
-- --     winPart =
-- --       Window.part model.nextId PartMsg winConfig 
-- --         [ Window.onClose (always <| CloseWindow model.nextId)
          
-- --         ]
-- --     model' =
-- --       { model
-- --         | nextId = model.nextId + 1
-- --         , windows = Dict.insert model.nextId winPart model.windows
-- --       }
-- --   in
-- --     model'

-- ACTION, UPDATE
type Msg
  = NewSize (Int, Int)
  | NewContext AppContext.Model
  | NewWindow (Maybe WindowConfig, ComponentConfig)
  | PartMsg (Parts.Msg PartStore Msg)
  | CloseWindow Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewSize (w, h) ->
      newSize model w h
    
    NewContext ctx ->
      newContext ctx model
    
    
    PartMsg msg' ->
      partMsg msg' model

    CloseWindow id ->
      closeWindow id model
    
    NewWindow (win, component) ->
        -- model |> noFx
      newWindow win component model


closeWindow : Int -> Model -> (Model, Cmd Msg)
closeWindow id model =
  let
    store =
      model.partStore
    store' =
      { store | window = Dict.remove id store.window }
    model' =
      { model 
        | partStore = store'
        , windows = Dict.remove id model.windows
      }
  in
    model' |> noFx
    

partMsg : Parts.Msg PartStore Msg -> Model -> (Model, Cmd Msg)
partMsg msg  model = 
  let
    (partStore', fx) =
      Parts.update PartMsg msg model.partStore
    model' =
      { model | partStore = partStore' }
  in
    (model', fx)


newSize : Model -> Int -> Int -> (Model, Cmd Msg)
newSize model w h =
  let
    wrapperHeight = h
    containerHeight = wrapperHeight - 15 --todo: remove magic number
    windowHeight = containerHeight - 10 --todo: remove magic number
    --todo: resize all windows
    (partStore', fxs) =
      Dict.values model.windows
      |> List.foldl
          (\part (store, fx) ->
              let
                (store', fx1) =
                  updatePart2 part.update (Window.NewSize (w, windowHeight)) store
              in
                (store', Cmd.batch [fx, fx1])
          )
          (model.partStore, Cmd.none)
      
      -- |> List.unzip
    -- windows' =
    --   List.map2 (,) (Dict.keys model.windows) windows
    --   |> Dict.fromList
    model' =
      { model
        | viewport = (w, h)
        , wrapperHeight = wrapperHeight
        , containerHeight = containerHeight
        , windowHeight = windowHeight
        , partStore = partStore'
      }
  in
    model' |> noFx


newContext : AppContext.Model -> Model -> (Model, Cmd Msg)
newContext ctx model =
  { model
    | context = Just ctx
  }
  |> noFx


newWindow : Maybe WindowConfig -> ComponentConfig -> Model -> (Model, Cmd Msg)
newWindow window component model =
  case model.context of
    Nothing ->
      -- todo: notify something
      let
        _ = "Context not initialized" |> Debug.log "content pane"
      in
        (model, Cmd.none)
    
    Just context ->
      let
        _ = (window, component) |> Debug.log "newWindow"
        (winPart, fx) =
          buildWindow context model.nextId model.windowHeight window component PartMsg
          -- Window.part model.nextId PartMsg (Window.init config' viewFn) 
            [ Window.onClose (always <| CloseWindow model.nextId) ]
        model' =
          { model
            | nextId = model.nextId + 1
            , windows = Dict.insert model.nextId winPart model.windows
          }
        
      in
        -- createWindow testConfig model
        (model', fx)
        -- |> noFx


-- VIEW setup
view : Model -> Html Msg
view model =
  div [ viewStyle model, class "content-pane" ]
    [ div [ contentStyle model, class "content" ]
        [ div [ class "window-container-wrapper", windowsWrapper model ]
            [ div [ class "window-container", windowsContainer model ]
                ( case model.context of
                    Just ctx ->
                      model.windows
                      |> Dict.values
                      |> List.map (\part -> part.view ctx model.partStore)
                    
                    Nothing ->
                      [ div [] [ text "Initializing" ]]
                )
            ]
        ]
    ]


-- styles
(=>) : a -> b -> (a,b)
(=>) = (,)


windowsWrapper : Model -> Attribute Msg
windowsWrapper model =
  let 
    heightPx = 
      (toString model.wrapperHeight) ++ "px"
  in
    style
      [ "height" => heightPx
      , "overflow-x" => "auto"
      , "overflow-y" => "hidden"
      ]


windowsContainer : Model -> Attribute Msg
windowsContainer model =
  let 
    heightPx = (toString model.containerHeight) ++ "px"
    width = model.windows
            |> Dict.values
            |> List.map (\part -> 15 + (part.get model.partStore |> .viewport |> fst))
            |> List.foldl (+) 0
    widthPx = (toString width) ++ "px"
  in
    style
      [ "height" => heightPx
      , "width" => widthPx
      ]


viewStyle : Model -> Attribute Msg
viewStyle model =
  style
    [ "margin-left" => "240px"
    , "overflow" => "hidden"
    , "background-color" => "Teal"
    ]


contentStyle : Model -> Attribute Msg
contentStyle model =
  style
    [ ("margin-bottom", "30px")
    , ("margin-top", "70px")
    , ("padding", "20px 5px 15px 5px")
    , "height" => ((toString <| snd model.viewport) ++ "px")
    , "background-color" => "teal"
    ]


-- PART
type alias Container c =
  { c | contentPanePanel : Maybe Model }


type alias Part container obs =
  Parts.Part Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instance1 view update .contentPanePanel (\x m -> { m | contentPanePanel = x})