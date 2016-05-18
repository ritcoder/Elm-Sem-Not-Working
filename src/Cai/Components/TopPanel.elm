module Cai.Components.TopPanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewContext)
  )


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.Context as AppContext
import App.Components.Calculator as Calculator
import App.Components.FeedBack as FeedBack
import App.Components.Authentication.UserInfoPanel as UserInfo
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : Maybe AppContext.Model
  , calculator : Maybe Calculator.Model
  , feedback : Maybe FeedBack.Model
  , userInfo : Maybe UserInfo.Model
  , activeMenu : Menu
  }


type Menu
  = None
  | Calculator
  | Trello
  | UserInfo


init : Maybe AppContext.Model -> Model
init ctx =
  { context = ctx
  , calculator = Nothing
  , feedback = Nothing
  , userInfo = Nothing
  , activeMenu = None
  }


-- ACTION, UPDATE
type Msg
  = PartMsg (Parts.Msg Model Msg)
  | ToggleMenu Menu
  | NewContext AppContext.Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PartMsg msg' ->
      partMsg msg' model
    
    ToggleMenu menu ->
      toggleMenu menu model |> noFx
    
    NewContext context ->
      newContext context model


toggleMenu : Menu -> Model -> Model
toggleMenu menu model =
  if menu == model.activeMenu then
    { model | activeMenu = None }
  else
    { model | activeMenu = menu }
      

partMsg : Parts.Msg Model Msg -> Model -> (Model, Cmd Msg)
partMsg msg model =
  Parts.update PartMsg msg model


newContext : AppContext.Model -> Model -> (Model, Cmd Msg)
newContext ctx model =
  { model | context = Just ctx }
  |> updatePart2 feedback.update (FeedBack.NewContext ctx)

-- VIEW setup
view : Model -> Html Msg
view model =
  div [ topBarStyle model, class "topbar" ]
    ( case model.context of
        Nothing ->
          [ div [] [ text "No context" ] ]
        
        Just ctx ->
          [ menuView ctx model ]
        
    )


menuView : AppContext.Model -> Model -> Html Msg
menuView ctx model =
  div [ class "ui text menu"] 
    [ div [ class "item", style [ ("padding-left", "20px") ] ] 
        [ img [ class "ui mini bordered image", src ctx.appInfo.smallLogo ] []]
    , div [ class "header item", style [ ("text-transform", "initial") ] ]
        [ text ctx.appInfo.name ]
    , div [ class "right menu" ] 
        [  div [ class "item" ] 
            [ div [ class "disabled ui labeled button" ] 
                [ div [ class "ui blue basic icon button" ] [ i [ class "alarm outline icon" ] [] ]
                , a [ class "ui blue left pointing label" ] [ text "0" ]
                ]
            ]
        , div [ class "ui dropdown item" ]
            [ div 
                [ classList 
                    [ ("circular purple ui icon toggle button", True)
                    , ("basic", model.activeMenu /= Calculator)
                    ]
                , onClick <| ToggleMenu Calculator
                ]
                [ i [ class "calculator icon" ] [] ]
            , div 
                [ class "ui purple menu segment"
                , style 
                    [ menuDisplayStyle Calculator model
                    , ("width", "300px")
                    , ("padding-top", "10px")
                    , ("margin-left", "-60px")
                    , ("margin-top", "18px")
                    , ("border-radius", "0")
                    ]
                ]
                [ calculator.view model ]
            ]
        , div [ class "item" ] 
            [ div [ class "disabled circular basic teal ui icon button" ] [ i [ class "icon comments outline" ] [] ] ]
        , div [ class "ui dropdown item" ]
            [ div 
                [ classList 
                    [ ("circular green ui icon toggle button", True)
                    , ("basic", model.activeMenu /= Trello)
                    ]
                , onClick <| ToggleMenu Trello
                ]
                [ i [ class "trello icon" ] [] ]
            , div 
                [ class "ui green menu segment"
                , style 
                    [ menuDisplayStyle Trello model
                    , ("width", "300px")
                    , ("padding-top", "10px")
                    , ("margin-left", "-160px")
                    , ("margin-top", "18px")
                    , ("border-radius", "0")
                    ]
                ]
                [ feedback.view model ]
            ]
        , div [ class "item" ] 
            [ div [ class "disabled circular basic purple ui icon button" ] [ i [ class "icon help" ] [] ] ]
        , div [ class "ui link dropdown item" ] 
            ( case ctx.user of
                Nothing ->
                  [ img [ class "ui mini link avatar image", src "/assets/images/user_pic.jpg" ] [] ]
                
                Just user ->
                  [ img 
                      [ classList 
                          [ ("ui mini link image", True)
                          , ("rounded", model.activeMenu == UserInfo)
                          , ("circular", model.activeMenu /= UserInfo)
                          ]
                      , src user.pic 
                      , onClick <| ToggleMenu UserInfo
                      ]
                      []
                  , div
                      [ class "ui orange menu segment"
                      , style 
                        [ menuDisplayStyle UserInfo model
                        , ("width", "200px")
                        , ("padding-top", "10px")
                        , ("margin-left", "-160px")
                        , ("margin-top", "11px")
                        , ("border-radius", "0")
                        ]
                    ]
                    [ userInfo.view ctx model ]
                  ]
            )                      
        ]
    ]


topBarStyle : Model -> Attribute Msg
topBarStyle model =
  style
    [ ("background", "#fff")
    , ("box-shadow", "1px 0 30px 0 rgba(0, 0, 0, 0.2)")
    , ("left", "0px")
    , ("right", "0px")
    , ("position", "fixed")
    , ("z-index", "1000")
    , ("height", "70px")
    ]


menuDisplayStyle : Menu -> Model -> (String, String)
menuDisplayStyle menu model =
  if model.activeMenu == menu then
    ("display", "block")
  else 
    ("display", "none")



calculator : Calculator.Part Model Msg
calculator =
  Calculator.part PartMsg Calculator.init []


feedback : FeedBack.Part Model Msg
feedback =
  FeedBack.part PartMsg (FeedBack.init Nothing)  []


userInfo : UserInfo.Part Model Msg
userInfo =
  UserInfo.part PartMsg (UserInfo.init Nothing) []


-- Part
type alias Container c =
  { c | topPanel : Maybe Model }


type alias Part container obs =
  Parts.Part Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instance1 view update .topPanel (\x m -> { m | topPanel = x})