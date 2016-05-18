module Semantic.Elements.Button exposing (..)


-- imports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Semantic.Messaging exposing (..)


-- model
init : Settings -> State -> Model
init settings state =
  { settings = settings
  , state = state
  }


type alias Model =
  { settings : Settings
  , state : State
  }


type alias Settings =
  { icon : Maybe String
  , text : String
  , klasses : String
  , name : String
  }


blankSettings : String -> Settings
blankSettings name =
  { icon = Nothing
  , text = ""
  , klasses = ""
  , name = name
  }


type alias State =
  { disabled : Bool }


blankState : State
blankState =
  { disabled = False }


-- update
update : Msg -> Model -> (Model, Cmd Msg, ComponentMessages)
update msg model =
  case msg of
    Submit ->
      submit model


submit : Model -> (Model, Cmd Msg, ComponentMessages)
submit model =
  ( model, Cmd.none, [ ButtonClick model.settings.name model.settings.text [] ] )


type Msg
  = Submit


-- view
view : Model -> Html Msg
view model =
  button 
    [ klass model
    , onClick Submit
    ]
    (renderBody model)


renderBody : Model -> List (Html Msg)
renderBody model =
  let
    t = text model.settings.text
    icon =
      case model.settings.icon of
        Nothing ->
          []
        Just icon ->
          [ i [class (icon ++ " icon")] [] ]
    view = --todo: order the views depending on the icon alignment
      t :: icon
  in
    view


klass : Model -> Attribute Msg
klass model =
  let
    baseClass = "ui button "
  in
    baseClass ++ model.settings.klasses |> class