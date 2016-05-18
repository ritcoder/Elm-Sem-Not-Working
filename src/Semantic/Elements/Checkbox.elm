module Semantic.Elements.Checkbox exposing
  ( Model, init, fromValue
  , Msg (ClearError, ResetValue, Validate), update
  , view
  , Type (.. )
  )


-- imports
-- import Cmd exposing (Cmd)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Semantic.Behaviours.FormValidation exposing (ValidationResult)
import Semantic.Messaging as SMessaging
import Utils exposing (..)


-- model
init : Settings -> State -> Bool -> Model
init settings state value =
  { value = value
  , settings = settings
  , state = state
  }


fromValue : Bool -> Model
fromValue =
  init blankSettings blankState


type alias Model =
  { value : Bool
  , settings : Settings
  , state : State
  }


blank : Bool -> Model
blank value =
  fromValue value


type alias Settings =
  { hasField : Bool
  , rules : List (Bool -> ValidationResult)
  , name : String
  , displayError: Bool
  , label : Maybe String
  , type' : Type
  }


blankSettings : Settings
blankSettings =
  { hasField = True
  , rules = []
  , name = ""
  , displayError = True
  , label = Just "label"
  , type' = Standard
  }


type alias State =
  { error : Maybe String }


type Type
  = Standard
  | Slider
  | Toggle


blankState : State
blankState =
  { error = Nothing }


name : String
name =
  "Check box"

-- update
update : Msg -> Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
update msg model =
  case msg of
    SetValue v->
      reaction2 name msg <| updateValue v model
    
    Validate ->
      reaction2 name msg <| validate model
    
    SetAndValidate v ->
      reaction2 name msg <| updateAndValidate v model
    
    ClearError ->
      reaction2 name msg <| clearError model
    
    ResetValue v ->
      reaction2 name msg <| resetValue v model


updateValue : Bool -> Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
updateValue x model =
  let
    msgs =
      case model.value == x of
        False ->
          [ SMessaging.BoolValueChanged model.settings.name x model.value ]
        True ->
          []
    model' =
      { model | value = x }
  in
    ( model', Cmd.none, msgs)


validate : Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
validate model =
  let
    state = 
      model.state
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
          [ SMessaging.FieldValidityChanged model.settings.name state'.error ]
    model' =
      { model | state = state' }
  in
    ( model', Cmd.none, msgs )


updateAndValidate : Bool -> Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
updateAndValidate x model =
  let
    (model', fx, msgs) =
      updateValue x model
    (model'', fx2, msgs2) =
      validate model'
    fx' 
      = Cmd.batch [fx, fx2]
    msgs' 
      = msgs ++ msgs2
  in
    (model'', fx', msgs')


clearError : Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
clearError model =
  let
    state = model.state
    state' =
      { state | error = Nothing }
  in
    ({ model | state = state' }, Cmd.none, [])


resetValue : Bool -> Model -> (Model, Cmd Msg, SMessaging.ComponentMessages)
resetValue v model =
  let
    (m, fx, msgs) =
      updateValue v model
    (m', fx2, msgs2) =
      clearError m
  in
    (m', Cmd.batch [fx, fx2], msgs ++ msgs2)


type Msg
  = SetValue Bool
  | ResetValue Bool
  | Validate
  | SetAndValidate Bool
  | ClearError


-- view
view : Model -> Html Msg
view model =
  let
    (checkbox, validation) = 
      ui model
  in
    case model.settings.hasField of
      False ->
        checkbox
      True ->
        div [ errorClass model.state.error "inline field" ]
          ( case validation of
              Just v ->
                [ checkbox, v ]
              Nothing ->
                [ checkbox ]              
          )


ui : Model -> (Html Msg, Maybe (Html Msg))
ui model =
  let
    inputLabel =
      case model.settings.label of
        Just x -> 
          label 
              [ onClick (SetAndValidate (not model.value)) ] 
              [ text x ]
          |> Just
        Nothing ->
          Nothing
    checkInput =
      input
        [ checked model.value
        , inputClass inputLabel
        , type' "checkbox"
        , onClick (SetValue (not model.value))
        , on "input" (Decode.map (always Validate) targetChecked)
        ]
        []
    checkbox =
      div [ errorClass model.state.error ("ui checkbox " ++ typeToString model.settings.type') ]
        ( case inputLabel of
            Just l ->
              [ checkInput, l ]
            Nothing ->
              [ checkInput ]
        )
    validation =
      case (model.settings.displayError, model.state.error) of 
        (True, Just errorMsg) ->
          div [ class "ui basic red pointing prompt label" ] [ text errorMsg ]
          |> Just
        _ ->
          Nothing
    in
      (checkbox, validation)


inputClass : Maybe (Html Msg) -> Attribute Msg
inputClass label =
  let
    klass =
      case label of
        Just _ ->
          "hidden"
        _ ->
          ""
  in
    class klass


errorClass : Maybe String -> String -> Attribute Msg
errorClass error otherClasses =
  case error of
    Nothing ->
      class otherClasses
    Just x ->
      "error " ++ otherClasses
      |> class


typeToString : Type -> String
typeToString x =
  case x of
    Slider ->
      "slider"
    Toggle ->
      "toggle"
    Standard ->
      ""