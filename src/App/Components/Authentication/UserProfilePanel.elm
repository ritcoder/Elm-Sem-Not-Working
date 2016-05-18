module App.Components.Authentication.UserProfilePanel exposing (..)


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeX exposing (decode, optional)

import App.Api as Api
import App.Context as AppContext
import Notification exposing (error, info)
import Parts
import Utils exposing (..)



-- MODEL
type alias Model =
  { context : AppContext.Model
  , data : Maybe Record
  }


init : AppContext.Model -> (Model, Cmd Msg)
init ctx =
  Model ctx Nothing
  |> flip (,) (toCmd2 Refresh)


name : String
name =
  "My Profile"

type alias Record =
  { name : String
  , email : String
  , role : String
  , ssn : String
  , department : String
  , phoneNumber : String
  }


decoder : Decoder Record
decoder =
  decode Record
  |> DecodeX.required "name" Decode.string
  |> DecodeX.optional "email" Decode.string ""
  |> DecodeX.required "role" Decode.string
  |> DecodeX.optional "ssn" Decode.string ""
  |> DecodeX.required "department" Decode.string
  |> DecodeX.optional "phoneNumber" Decode.string ""


-- UPDATE
type Msg
  = Refresh
  | NewData (Result String Record)
  
  | Busy String
  | Idle


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Refresh ->
      refresh model
    
    NewData data ->
      newData data model
    
    _ ->
      model |> noFx


refresh : Model -> (Model, Cmd Msg)
refresh model =
  let
    http =
      Api.get (Just model.context) "Users/Me2" decoder
      |> toCmd NewData
    busy =
      toCmd2 <| Busy "Loading..."
  in
    (model, Cmd.batch [http, busy])


newData : Result String Record -> Model -> (Model, Cmd Msg)
newData resp model =
  case resp of
    Ok user ->
      let
        model' =
          { model | data = Just user }
      in
        (model', toCmd2 Idle)
    
    Err msg ->
      let
        idle = toCmd2 Idle
        alert = error msg name
        fx =
          Cmd.batch [idle, alert]
      in
        (model, fx)


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  case (context.user, model.data) of
    (Just user, Just data) ->
      div 
        [ class "ui middle aligned center aligned grid"
        , style [ ("margin-top", "20px") ]
        ]
        [ div [ class "ui card"]
            [ div [ class "image" ] [ img [ src user.pic ] [] ]
            , div [ class "content" ]
                [ a [ class "header" ] [ text data.name ]
                , div [ class "meta"] [ span [] [ text data.role] ]
                , div [ class "description" ] [ text <| describe data ]
                ]
            , div [ class "extra content" ]
                [ span [] 
                    [ i [class "phone icon" ] []
                    , text data.phoneNumber
                    ]
                ]
            ]
        ]
      
    _ ->
      div [ class "ui error message" ]
        [ text "Initializing" ]


describe : Record -> String
describe data =
  if data.department == "" then
    "I have no department"
  else
    "My current department is " ++ data.department



-- COMPONENT
type alias Container c =
  { c | userProfile : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .userProfile (\x m -> { m | userProfile = x})


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
  { icon = "user", title = "My Profile", subTitle = "",  width = 400, height = 0 }