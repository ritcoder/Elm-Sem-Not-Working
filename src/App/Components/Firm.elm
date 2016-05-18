module App.Components.Firm exposing (..)


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeX exposing (decode, optional)

import App.Api as Api
import App.Context as AppContext
import Notification exposing (error)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : AppContext.Model
  , data : Firm
  , initialized : Bool
  }


init : AppContext.Model -> (Model, Cmd Msg)
init ctx =
  Model ctx emptyRecord False
  |> flip (,) (toCmd2 Refresh)


type alias Firm =
  { name : String
  , notes : String
  , email : String
  , smsName : String
  , address : String
  , logo : String
  }


firmDecoder : Decoder Firm
firmDecoder =
  decode Firm
  |> DecodeX.required "name" Decode.string
  |> DecodeX.required "notes" Decode.string
  |> DecodeX.required "email" Decode.string
  |> DecodeX.required "smsName" Decode.string
  |> DecodeX.required "address" Decode.string
  |> optional "logo" Decode.string ""


emptyRecord : Firm
emptyRecord =
  { name = "", notes = "", email = ""
  , smsName = "", address = "", logo = ""
  }


-- UPDATE
type Msg
  = Refresh
  | NewData (Result String Firm)
  | NewContext AppContext.Model
  
  | Close
  | Idle
  | Busy String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Refresh ->
      refresh model
    
    NewData data ->
      newData data model
    
    NewContext context ->
      newContext context model
    
    Close ->
      model |> noFx
    
    Idle ->
      model |> noFx
    
    Busy text ->
      model |> noFx


refresh : Model -> (Model, Cmd Msg)
refresh model =
  let
    cmd =
      Api.get (Just model.context) "Users/Company" firmDecoder
      |> toCmd NewData
  in
    ( model, Cmd.batch [cmd, toCmd2 <| Busy "Please wait..." ] )


newData : Result String Firm -> Model -> (Model, Cmd Msg)
newData data model =
  case data of
    Err msg -> --todo
      ( model, Cmd.batch [ error msg "Firm", toCmd2 Idle ])
    
    Ok d ->
      ( { model 
          | data = d
          , initialized = True
        }
      , toCmd2 Idle
      )


newContext : AppContext.Model -> Model -> (Model, Cmd Msg)
newContext context model =
  let
    model' =
      { model | context = context }
    fx =
      if model.initialized then
        Cmd.none
      else
        toCmd2 Refresh
  in
    ( model', fx)


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  div [ class "ui form" ] 
    [ div [ class "ui icon button", onClick Refresh ]
        [ i [ class "refresh icon" ] [] ]
    , div [ class "ui divider" ] []
    , div []
        [ img 
            [ classList 
                [ ("ui bordered centered small image", True)
                , ("hidden", case model.data.logo of
                              "" -> True
                              _ -> False
                  )
                ]
                
            , src <| "data:image/png;base64," ++ model.data.logo 
            ]
            []
        ]
    , div [ class "field" ]
        [ label [] [ text "Name" ]
        , input
            [ value model.data.name, readonly True ]
            []
        ]
    , div [ class "field" ]
        [ label [] [ text "Notes" ]
        , textarea
            [ rows 2
            , value model.data.notes
            , readonly True
            ]
            []
        ]
    , div [ class "ui segment" ]
        [ div [ class "teal ui ribbon label", ribbonStyle ] [ text "Contact & Communication"]
        , div [ class "field" ]
            [ label [] [ text "Email" ]
            , input
                [ rows 4
                , value model.data.email
                , readonly True
                ]
                []
            ]
        , div [ class "field" ]
            [ label [] [ text "SMS Name" ]
            , input
                [ readonly True
                , value model.data.smsName
                ]
                []
            ]
        , div [ class "field" ]
            [ label [] [ text "Address" ]
            , textarea
                [ rows 4
                , value model.data.address
                , readonly True
                ]
                []
            ]        
        ]
    ]


ribbonStyle : Attribute Msg
ribbonStyle =
  style [ ("margin-bottom", "15px")]


-- COMPONENT
type alias Container c =
  { c | firm : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .firm (\x m -> { m | firm = x})


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