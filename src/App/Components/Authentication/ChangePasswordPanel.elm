module App.Components.Authentication.ChangePasswordPanel exposing (..)


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

import App.Api as Api
import App.Context as AppContext
import Notification exposing (error, info)
import Parts
import Utils exposing (..)



-- MODEL
type alias Model =
  { context : AppContext.Model
  , record : Record
  , error : String
  }


init : AppContext.Model -> (Model, Cmd Msg)
init ctx =
  Model ctx emptyRecord "" |> noFx


name : String
name =
  "Change Password"

type alias Record =
  { password : String
  , newPassword : String
  , passwordConfirmation : String
  }

type Field
  = Password
  | NewPassword
  | PasswordConfirmation


emptyRecord : Record
emptyRecord =
  Record "" "" ""


encodeRecord : Record -> Encode.Value
encodeRecord record =
  Encode.object
    [ ("oldPassword", Encode.string record.password )
    , ("newPassword", Encode.string record.newPassword )
    , ("confirmPassword", Encode.string record.passwordConfirmation)
    ]


-- UPDATE
type Msg
  = SetField Field String
  | ChangePassword
  | ChangePasswordResponse (Result String String)
  
  | Busy String
  | Idle
  | Close


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetField field value ->
      setField field value model |> noFx
    
    ChangePassword ->
      changePassword model
    
    ChangePasswordResponse resp ->
      changePasswordResponse resp model
    
    _ ->
      model |> noFx


setField : Field -> String -> Model -> Model
setField field value model =
  let
    record = model.record
    record' =
      case field of
        Password ->
          { record | password = value }   
               
        NewPassword ->
          { record | newPassword = value }    
              
        PasswordConfirmation ->
          { record | passwordConfirmation = value }
    error = validate record'
    model' =
      { model 
        | record = record'
        , error = error
      }
  in
    model'


validate : Record -> String
validate record =
    if record.password == "" || 
       record.newPassword == ""
    then
      "All fields are required"
    else if record.newPassword /= record.passwordConfirmation then
      "New password and confirmation must be the same"
    else
      ""
      

changePassword : Model -> (Model, Cmd Msg)
changePassword model =
  case validate model.record of
    "" ->
      let
        fx =
          Api.post (Just model.context) "Account/ChangePassword" (encodeRecord model.record) Api.messageOrStringResponse
          |> toCmd ChangePasswordResponse
        busy =
          toCmd2 <| Busy "Please wait..."
      in
        (model, Cmd.batch [fx, busy])
    
    msg ->
      { model | error = msg } |> noFx


changePasswordResponse : Result String String -> Model -> (Model, Cmd Msg)
changePasswordResponse resp model =
  case resp of
    Ok msg ->
      let 
        close = toCmd2 Close
        alert = info (if msg == "" then "Password changed successfully" else msg) name
        fx =
          Cmd.batch [close, alert]
      in
        (model, fx)
    
    Err msg ->
      let
        record = model.record
        record' =
          record
          -- { record
          --   | password = ""
          --   , newPassword = ""
          --   , passwordConfirmation = ""
          -- }
        model' =
          { model
            | record = record'
            , error = msg
          }
        idle = toCmd2 Idle
        alert = error msg name
        fx =
          Cmd.batch [idle, alert]
      in
        (model', fx)


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  div [] 
    [ div [ class "ui form" ]
        [ div [ class "field" ]
            [ label [] [ text "Password*"]
            , input 
                [ type' "password"
                , value model.record.password
                , placeholder "Current Password"
                , onInput <| SetField Password
                ]
                []
            ]
        , div [ class "field" ]
            [ label [] [ text "New Password*" ]
            , div [ class "two fields" ]
                [ div [ class "field" ]
                    [ input 
                        [ type' "password"
                        , value model.record.newPassword
                        , placeholder "New Password"
                        , onInput <| SetField NewPassword
                        ]
                        []
                    ]
                , div [ class "field" ]
                    [ input 
                        [ type' "password"
                        , value model.record.passwordConfirmation
                        , placeholder "Password Confirmation"
                        , onInput <| SetField PasswordConfirmation
                        ]
                        []
                    ]
                ]              
            ]
        , button 
            [ class "ui right floated teal button", onClick ChangePassword ]
            [ text "Change Password" ]
        , br [] []
        ]
    , div 
        [ class <| "ui message " ++ if model.error == "" then "hidden" else "negative"
        , style [ ("margin-top", "25px") ]
        ]
        [ text <| model.error ]
    -- , div [] [ text <| toString model ]
    ]



-- COMPONENT
type alias Container c =
  { c | changePassword : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .changePassword (\x m -> { m | changePassword = x})


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


winConfig =
  { icon = "protect", title = "Change Password", subTitle = "",  width = 400, height = 0 }