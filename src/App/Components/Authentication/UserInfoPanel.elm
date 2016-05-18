module App.Components.Authentication.UserInfoPanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewContext)
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.AllPorts as System exposing (logout, newWindow)
import App.Context as AppContext
import App.ComponentList exposing (..)
import Notification exposing (info)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : Maybe AppContext.Model }


init : Maybe AppContext.Model -> Model
init context =
  { context = context }


-- UPDATE
type Msg
  = NewContext AppContext.Model
  | OpenWindow (Maybe WindowConfig) Component
  | Logout


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewContext context ->
      newContext context model |> noFx
    
    Logout ->
      ( model, Cmd.batch [ logout (), info "This will in a later update get confirmation first" "Not implemented"] )
    
    OpenWindow winConfig component ->
      ( model, newWindow winConfig component )


newContext : AppContext.Model -> Model -> Model
newContext context model =
  { model | context = Just context }


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div 
    [ style [ ("padding", "10px") ] ]
    [ div [ class "ui animated divided list" ]
        [ div 
            [ class "link item"
            , onClick <| OpenWindow Nothing UserProfile
            ]
            [ i [ class "user icon" ] []
            , div [ class "content" ]
                [ text "My Profile" ]
            ]
        , div 
            [ class "link item"
            , onClick <| OpenWindow Nothing UserSettings
            ]
            [ i [ class "setting icon" ] []
            , div [ class "content" ]
                [ text "My Settings" ]
            ] 
        , div 
            [ class "link item"
            , onClick <| OpenWindow Nothing ChangePassword
            ]
            [ i [ class "user icon" ] []
            , div [ class "content" ]
                [ text "Change Password" ]
            ] 
        , div 
            [ class "link item", onClick Logout ]
            [ i [ class "sign out icon" ] []
            , div [ class "content" ]
                [ text "Log out" ]
            ] 
        ]
    ]


createHeader : Model -> Html Msg
createHeader model =
  div [ class "header" ]
    [ i [ class "spy icon" ] []
    , text "User Options"
    ]


-- COMPONENT
type alias Container c =
  { c | userInfo : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .userInfo (\x m -> { m | userInfo = x})