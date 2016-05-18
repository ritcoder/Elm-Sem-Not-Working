module Semantic.Elements.Input exposing
  ( Model, init, fromValue
  , Msg (ClearError, ResetValue, Validate), update
  , view
  )


-- imports
-- import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Semantic.Behaviours.FormValidation exposing (ValidationResult, valid)
import Semantic.Messaging as Messaging exposing (ComponentMessages)
import Utils exposing (..)


-- model
init : Settings -> State -> String -> Model
init settings state value =
  { value = value
  , settings = settings
  , state = state
  }


fromValue : String -> Model
fromValue =
  init blankSettings blankState


type alias Model =
  { value : String
  , settings : Settings
  , state : State
  }


blank : String -> Model
blank value =
  init blankSettings blankState value


type alias Settings =
  { hasField : Bool
  , type' : String
  , placeholder : String
  , label : Maybe String
  , rules : List (String -> ValidationResult)
  , name : String
  , displayError : Bool
  }
  

blankSettings : Settings
blankSettings =
  { hasField = True
  , type' = "text"
  , placeholder =""
  , label = Just ""
  , rules = []
  , name = ""
  , displayError = True
  }


type alias State =
  { error : Maybe String }


blankState : State
blankState =
  { error = Nothing }


-- update
update : Msg -> Model -> (Model, Cmd Msg, ComponentMessages)
update msg model =
  case msg of
    SetValue v ->
      reaction2 "input" msg <| updateValue v model
    
    Validate ->
      reaction2 "input" msg <| validate model
    
    ClearError ->
      reaction2 "input" msg <| clearError model
    
    ResetValue v ->
      reaction2 "input" msg <| resetValue v model


updateValue : String -> Model -> (Model, Cmd Msg, ComponentMessages)
updateValue x model =
  let 
    msgs = 
      case model.value == x of
        False ->
          [ Messaging.TextValueChanged model.settings.name x model.value ]
        True ->
          []
    model' = { model | value = x }
  in
    ( model', Cmd.none, msgs )


clearError : Model -> (Model, Cmd Msg, ComponentMessages)
clearError model =
  let
    state = model.state
    state' =
      { state | error = Nothing }
  in
    ({ model | state = state' }, Cmd.none, [])


resetValue : String -> Model -> (Model, Cmd Msg, ComponentMessages)
resetValue v model =
  let
    (m, fx, msgs) =
      updateValue v model
    (m', fx2, msgs2) =
      clearError m
  in
    (m', Cmd.batch [fx, fx2], msgs ++ msgs2)


validate : Model -> (Model, Cmd Msg, ComponentMessages)
validate model = 
  let
    state = model.state
    state' =
      model.settings.rules
      |> List.map (\rule -> rule model.value)
      |> List.filter (\result -> not result.isValid)
      |> List.head
      |> Maybe.map (\x -> x.message)
      |> \x ->
            { state | error = x }
    msgs =
      case state.error == state'.error of
        True ->
          []
        False ->
          [ Messaging.FieldValidityChanged model.settings.name state'.error ]
                
    model' = { model | state = state'}
  in
    (model', Cmd.none, msgs)



type Msg
  = SetValue String
  | ResetValue String
  | Validate
  | ClearError


-- view
view : Model -> Html Msg
view model =
  div [ klass model ]
    ( ui model )


ui : Model -> List (Html Msg)
ui model =
  let
    base =
      case model.settings.label of
        Just x -> [ label [] [text x ] ]
        Nothing -> []
        
    textBox =
      createInput model
      
    view =
      case (model.settings.displayError, model.state.error) of
        (True, Just errorMsg) ->
          [ textBox
          , div [ class "ui basic red pointing prompt label" ] [ text errorMsg ]
          ]
        _ -> 
          [ textBox ]            
  in
    base ++ view
      
 
createInput : Model -> Html Msg
createInput model =
  case model.settings.type' of
    "textarea" ->
      textarea
        [ value model.value
        , rows 4
        , placeholder model.settings.placeholder
        , onInput SetValue
        , onBlur Validate
        , on "keyup" (Decode.succeed Validate)
        ]
        []
      
    _ ->
      input 
        [ value model.value
        , placeholder model.settings.placeholder
        , type' model.settings.type'
        , onInput SetValue
        , onBlur Validate
        , on "keyup" (Decode.succeed Validate)
        ] 
        []

klass model =
  let
    baseClass = 
      if model.settings.hasField then
        "field"
      else
        "ui input"
    error =
      case model.state.error of
        Just x ->
          " error"
        Nothing ->
          ""
  in
    baseClass ++ error |> class