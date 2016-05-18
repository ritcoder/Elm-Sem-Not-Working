module App.Components.Authentication.LoginPanel exposing
  ( Model, init, update
  , Part, part
  , view
  , Msg (NewContext)
  )

import Html exposing (..)
import Html.Events exposing (..)
import App.Context as AppContext
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { v : Int }


init : Model
init =
  Model 0


-- UPDATE
type Msg
  = NewContext AppContext.Model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewContext context ->
      model |> noFx


-- VIEW setup
view : AppContext.Model -> Model -> Html Msg
view ctx model =
  div [] 
    [ text "Hurray!! Login form here!" ]


-- COMPONENT
type alias Container c =
  { c | loginPanel : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .loginPanel (\x m -> { m | loginPanel = x})