module Cai.Components.SideBarPanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewContext)
  )


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.AllPorts exposing (newWindow)
import App.Context as AppContext
import App.ComponentList exposing (..)

import Cai.Common exposing (..)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : Maybe AppContext.Model }


init : Maybe AppContext.Model -> Model
init  context =
  { context = context}


-- Msg, UPDATE
type Msg
  = OpenWindow (Maybe WindowConfig) Component
  | NewContext AppContext.Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewContext context ->
      newContext context model |> noFx
    
    OpenWindow winConfig component ->
      ( model, newWindow winConfig component )


newContext : AppContext.Model -> Model -> Model
newContext context model =
  { model | context = Just context }


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div 
    [ class "sidebar", sideBarStyle model ]
    [ createUserDetails ctx model
    , createMenuItems ctx model
    ]


createUserDetails : AppContext.Model -> Model -> Html Msg
createUserDetails ctx model =
  let
    (username, pic, role) =
        case ctx.user of
            Nothing ->
                ( "", "", "" )
            
            Just u ->
                ( u.userName, u.pic, u.role )
  in
  div [ class "user-details", style [ ("margin-top", "30px") ]] 
    [ div []
        [ h3 [ class "ui dividing header" ]
            [ img [ class "ui mini link avatar image", src pic ] []
            , div [ class "content" ]
                [ text username
                , div [ class "sub header" ]
                    [ text role ]
                ]
            ]
        ] 
    ]


-- todo: get menu as part of model
createMenuItems : AppContext.Model -> Model -> Html Msg
createMenuItems ctx model =
  div [ class "ui vertical menu" ]
    [ div [ class "item" ]
        [ div [ class "header" ] [text "Case Managment"]
        , div [ class "menu" ]
            [ a [ class "disabled active red item" ]
                [ i [ class "file icon" ] []
                , text "New Case"
                ]
            , a [ class "disabled active red item" ]
                [ i [ class "find icon" ] []
                , text "Case Finder"
                ]
            , a [ class "disabled active red item" ] 
                [ i [ class "book icon" ] []
                , text "Case Contacts"
                ]
            ]
        ]
    , div [ class "item" ]
        [ div [ class "header" ] [text "General"]
        , div [ class "menu" ]
            [ a [ class "disabled active red item" ] 
                [ i [ class "tags icon" ] []
                , text "Utilties"
                ]
            , a [ class "disabled active red item" ]
                [ i [ class "options icon" ] []
                , text "Plugins"
                ]
            , a [ class "disabled active red item" ]
                [ i [ class "list layout icon" ] []
                , text "Lookups"
                ]
            ]
        ]
    , div [ class "item" ]
        [ div [ class "header" ] [ text "Setup & Admin" ]
        , div [ class "menu" ]
            [ a [ class "disabled active red item" ] 
                [ i [ class "settings icon" ] []
                , text "Settings"
                ]
            , a [ class "blue active item"
                , onClick <| OpenWindow Nothing ServiceList
                ] 
                [ i [ class "random icon" ] []
                , text "Service List"
                ]
            ]
        ]
    , div [ class "item" ]
        [ div [ class "header" ] [text "Organization"]
        , div [ class "menu" ]
            [ a [ class "disabled active red item" ] 
                [ i [ class "users icon" ] []
                , text "Staff / Members"
                ]
            , a [ class "active purple item"
                , onClick <| OpenWindow Nothing IdTypes
                ] 
                [ i [ class "sitemap icon" ] []
                , text "Branches"
                ]
            , a [ class "active blue item" 
                , onClick <| OpenWindow Nothing Firm
                ] 
                [ i [ class "block layout icon" ] []
                , text "Firm"
                ]
            ]
        ]
    , div [ class "item" ]
        [ div [ class "header" ] [text "Reports"]
        , div [ class "menu" ]
            [ a [ class "disabled active red item" ] 
                [ i [ class "bar chart icon" ] []
                , text "General"
                ]
            , a [ class "disabled active red item" ]
                [ i [ class "pie chart icon" ] []
                , text "Ad-hoc"
                ]
            ]
        ]
    ]


sideBarStyle : Model -> Attribute Msg
sideBarStyle model =
  style
    [ ("bottom", "50px")
    , ("top", "40px")
    , ("height", "100%")
    , ("width", "240px")
    , ("margin-bottom", "-80px")
    , ("margin-top", "30px")
    , ("padding-bottom", "80px")
    , ("padding-left", "10px")
    , ("position", "fixed")
    , ("background", "#ffffff")
    , ("box-shadow", "0 1px 3px rgba(0,0,0,0.15)")
    ]


-- PART
type alias Container c =
  { c | sideBarPanel : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .sideBarPanel (\x m -> { m | sideBarPanel = x})