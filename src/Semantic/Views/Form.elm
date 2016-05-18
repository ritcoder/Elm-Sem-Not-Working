module Semantic.Views.Form exposing (..)


-- imports
import Dict exposing (Dict)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Semantic.Behaviours.FormValidation exposing (ValidationResult, notEmpty)
import Semantic.Elements.Button as SButton
import Semantic.Elements.Checkbox as SCheckbox
import Semantic.Elements.File as SFile
import Semantic.Elements.Input as SInput
import Semantic.Messaging as SMessaging exposing (ComponentMessage, ComponentMessages)
import Semantic.Modules.Dropdown as SDropdown
import String
import Utils exposing (..)


-- model
init : Settings value fieldset -> State -> value -> Model value fieldset
init settings state value =
  let
    (fields, fx) = 
      updateFieldValues settings.fields settings.fieldNames settings.getField 
        settings.setField settings.getRecordValue value
    settings' = { settings | fields = fields }
  in
    { value = value
    , settings = settings'
    , state = state
    }


validate : Model value fieldset -> Model value fieldset
validate model =
  let
    settings = model.settings
    state = model.state
    (fields', errors) =
      settings.fieldNames
      |> List.foldl
          (\name (fs, errs) ->
              let
                field = settings.getField model.settings.fields name
                (field', name, error) = validateField field
                errs' = 
                  case error of
                    Just err ->
                      Dict.insert name err errs
                    Nothing ->
                      Dict.remove name errs
              in
                (settings.setField fs name field', errs')
          )
          (settings.fields, state.errors)
    settings' = { settings | fields = fields' }
    state' = { state | errors = errors }
  in
    { model |
        settings = settings',
        state = state'
    }

  
updateFieldValues : 
  fieldset -> List String -> 
  (fieldset -> String -> Field) -> (fieldset -> String -> Field -> fieldset) ->
  (value -> String -> FieldValue) ->
  value -> (fieldset, Cmd (Msg v))        
updateFieldValues fields fieldNames getField setField getValue value =
  let
    (fields', fxs') =
      fieldNames
      |> List.foldl 
          (\name (fs, fxs) ->
              let
                field = getField fields name
                v = getValue value name 
                (field', fx) = setFieldValue field v --todo: validate the field or clear all validation
              in
                (setField fs name field', fx :: fxs)
          )
          (fields, [])
  in
    (fields', Cmd.batch fxs')


setFieldValue : Field -> FieldValue -> (Field, Cmd (Msg v))
setFieldValue field value =
  case field of
    None -> (field, Cmd.none)
    
    Button m -> (field, Cmd.none)
    
    Input m ->
      let
        (m', fx, _) =
          SInput.update (SInput.ResetValue (toText m.value value)) m
      in
        (Input m', Cmd.map (UpdateInput m'.settings.name) fx)
    
    Checkbox m ->
      let
        (m', fx, _) =
          SCheckbox.update (SCheckbox.ResetValue (toBool m.value value)) m
      in
        (Checkbox m', Cmd.map (UpdateCheckbox m'.settings.name) fx)
        
    Dropdown m ->
      let
        (m', fx, _) =
          SDropdown.update (SDropdown.ResetValue (toText (Maybe.withDefault "" m.state.value) value |> Just)) m
      in
        (Dropdown m', Cmd.map (UpdateDropdown m'.settings.name) fx)
    
    File m ->
      let
        (m', fx, _) =
          SFile.update (SFile.ResetValue "") m --(toText m.value value)
      in
        (File m', Cmd.map (UpdateFile m'.settings.name) fx)


getFieldName : Field -> Maybe String
getFieldName field =
  case field of
    None -> Nothing
    Button m ->
      Just m.settings.name
    Input m ->
      Just m.settings.name
    Checkbox m ->
      Just m.settings.name
    Dropdown m ->
      Just m.settings.name
    File m ->
      Just m.settings.name


type alias Model value fieldset =
  { value : value
  , settings: Settings value fieldset
  , state : State
  }


validateField : Field -> (Field, String, Maybe String)
validateField field =
  let
    na = (field, "", Nothing)
  in
    case field of
      Input m ->
        let
          (m',_, _) = SInput.update SInput.Validate m
        in
          (Input m', m'.settings.name, m'.state.error)
      
      Checkbox m ->
        let
          (m',_, _) = SCheckbox.update SCheckbox.Validate m
        in
          (Checkbox m', m'.settings.name, m'.state.error)
      
      Dropdown m ->
        let
          (m',_, _) = SDropdown.update SDropdown.Validate m
        in
          (Dropdown m', m'.settings.name, m'.state.error)
      
      File m ->
        let
          (m', _, _) = SFile.update SFile.Validate m
        in
          (File m', m'.settings.name, m'.state.error)
      
      Button m -> na
      None -> na


blank : value -> Model value {}
blank value =
  init blankSettings blankState value


type alias Settings value fieldset =
  { fields : fieldset
  , renderer : ModelRef value fieldset -> Html (Msg value)
  , setRecordValue : value -> String -> FieldValue -> value
  , getRecordValue : value -> String -> FieldValue -- List (String, value -> FieldValue) 
  , setField : fieldset -> String -> Field -> fieldset
  , getField : fieldset -> String -> Field
  , fieldNames : List String
  }


blankSettings : Settings value {}
blankSettings =
  { fields = {}
  , renderer = \m -> div [] [ text "Renderer not found" ]
  , setRecordValue = \f n v -> f
  , getRecordValue = \v n -> NoValue
  , setField = \fs n f -> fs
  , getField = \fs n -> None
  , fieldNames = []
  }


type alias State =
  { errors : Dict String String
  }


blankState : State
blankState =
  { errors = Dict.empty
  }


type ModelRef value fieldset
  = ModelRef (Model value fieldset)


name : String
name =
  "Form"


-- update
update : Msg value -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
update msg model =
  case msg of
    UpdateInput name act ->
      reaction2 name msg <| updateInput name act model
      
    UpdateButton name act ->
      reaction2 name msg <| updateButton name act model
    
    UpdateCheckbox name act ->
      reaction2 name msg <| updateCheckbox name act model
    
    UpdateDropdown name act ->
      reaction2 name msg <| updateDropdown name act model
    
    UpdateValue value ->
      reaction2 name msg <| updateValue value model
    
    UpdateFile name act ->
      reaction2 name msg <| updateFile  name act model


updateFile : String -> SFile.Msg -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
updateFile name msg model =
  let
    fields = model.settings.fields
    settings = model.settings
    field = settings.getField fields name
    value = model.value
    (field', value', fx, msgs) = 
      case field of
        File fileModel ->
          let
            (m', fx, msgs) = SFile.update msg fileModel
            value' = settings.setRecordValue value name NoValue -- (TextValue m'.value)
            --todo: process any messages
            --todo: update to make the update only when the field value changes
            --todo: update to set the validation status when the message is recieved
          in  
            (File m', value', Cmd.map (UpdateFile name) fx, msgs)
        
        _ -> (field, value, Cmd.none, [])
    fields' = settings.setField fields name field'
    model' =
      let
        settings' = { settings | fields = fields' }
      in
        { model | settings = settings', value = value' }
  in
    (model', fx, msgs)


updateButton : String -> SButton.Msg -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
updateButton name msg model =
  let
    fields = model.settings.fields
    settings = model.settings
    field = settings.getField fields name
    (field', fx, msgs) = 
      case field of
        Button m ->
          let
            (m', fx, msgs) = SButton.update msg m
          in  
            (Button m', Cmd.map (UpdateButton m'.settings.name) fx, msgs)
        
        _ -> (field, Cmd.none, [])
    fields' = settings.setField fields name field'
    model' =
      let
        settings' = { settings | fields = fields' }
      in
        { model | settings = settings' }
  in
    ( model', fx, msgs )


updateInput : String -> SInput.Msg -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
updateInput name msg model =
  let
    fields = model.settings.fields
    settings = model.settings
    field = settings.getField fields name
    value = model.value
    (field', value', fx, msgs) = 
      case field of
        Input inputModel ->
          let
            (m', fx, msgs) = SInput.update msg inputModel
            value' = settings.setRecordValue value name (TextValue m'.value)
            --todo: process any messages
            --todo: update to make the update only when the field value changes
            --todo: update to set the validation status when the message is recieved
          in  
            (Input m', value', Cmd.map (UpdateInput name) fx, msgs)
        
        _ -> (field, value, Cmd.none, [])
    fields' = settings.setField fields name field'
    model' =
      let
        settings' = { settings | fields = fields' }
      in
        { model | settings = settings', value = value' }
  in
    (model', fx, msgs)


updateDropdown : String -> SDropdown.Msg -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
updateDropdown name msg model =
  let
    fields = model.settings.fields
    settings = model.settings
    field = settings.getField fields name
    value = model.value
    (field', value', fx, msgs) = 
      case field of
        Dropdown inputModel ->
          let
            (m', fx, msgs) = SDropdown.update msg inputModel
            value' = settings.setRecordValue value name (Maybe.withDefault "" m'.state.value |> TextValue)
            --todo: process any messages
            --todo: update to make the update only when the field value changes
            --todo: update to set the validation status when the message is recieved
          in  
            (Dropdown m', value', Cmd.map (UpdateDropdown name) fx, msgs)
        
        _ -> (field, value, Cmd.none, [])
    fields' = settings.setField fields name field'
    model' =
      let
        settings' = { settings | fields = fields' }
      in
        { model | settings = settings', value = value' }
  in
    (model', fx, msgs)

updateCheckbox : String -> SCheckbox.Msg -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)
updateCheckbox name msg model =
  let
    fields = model.settings.fields
    settings = model.settings
    field = settings.getField fields name
    value = model.value
    (field', value', fx, msgs) = 
      case field of
        Checkbox inputModel ->
          let
            (m', fx, msgs) = SCheckbox.update msg inputModel
            value' = settings.setRecordValue value name (BoolValue m'.value)
            --todo: process any messages
            --todo: update to make the update only when the field value changes
            --todo: update to set the validation status when the message is recieved
          in  
            (Checkbox m', value', Cmd.map (UpdateCheckbox name) fx, msgs)
        
        _ -> (field, value, Cmd.none, [])
    fields' = settings.setField fields name field'
    model' =
      let
        settings' = { settings | fields = fields' }
      in
        { model | settings = settings', value = value' }
  in
    (model', fx, msgs)


updateValue : value -> Model value fieldset -> (Model value fieldset, Cmd (Msg value), ComponentMessages)  
updateValue value model =
   let
    settings =
      model.settings
    (fields, fx) = 
      updateFieldValues settings.fields settings.fieldNames settings.getField 
        settings.setField settings.getRecordValue value
    settings' = { settings | fields = fields }
    model' = 
      { model | settings = settings', value = value }
  in
    (model', fx, [])


noFx : a -> (a, Cmd (Msg value))
noFx x =
  (x, Cmd.none)

type Msg value
  = UpdateInput String SInput.Msg
  | UpdateFile String SFile.Msg
  | UpdateButton String SButton.Msg
  | UpdateCheckbox String SCheckbox.Msg
  | UpdateDropdown String SDropdown.Msg
  | UpdateValue  value


-- view
view : Model value fieldset -> Html (Msg value)
view model =
  model.settings.renderer (ModelRef model)


--todo: make part of form
renderField : Model value fieldset -> Field -> Html (Msg value)
renderField model field =
  case field of
    Input m ->
      Html.map (UpdateInput m.settings.name) (SInput.view  m)
    
    Button m ->
      Html.map (UpdateButton m.settings.name) (SButton.view  m)
    
    Checkbox m ->
      Html.map (UpdateCheckbox m.settings.name) (SCheckbox.view  m)

    Dropdown m ->
      Html.map (UpdateDropdown m.settings.name) (SDropdown.view  m)
    
    File m ->
      Html.map (UpdateFile m.settings.name) (SFile.view  m)
    
    None ->
      text ""


textField : String -> String -> String -> String -> List ( String -> ValidationResult) -> Field
textField name label type' placeholder rules =
  let 
    state =
      { error = Nothing }
    settings =
      { hasField = True
      , type' = type'
      , placeholder = placeholder
      , label = Just label
      , rules = rules
      , name = name
      , displayError = True
      }
  in
    SInput.init settings state "" |> Input


fileField : String -> String -> String -> Bool -> Bool -> Field
fileField name label prompt multiple required =
  let
    state = SFile.blankState
    state' =
      { state | error = Nothing }
    settings = SFile.blankSettings
    settings' =
      { settings
          | hasField = True
          -- , type' = "file"
          , placeholder = prompt
          , name = name
          , label = Just label
          -- , rules = []
          , displayError = True
      }
  in
    SFile.init settings' state' |> File --todo: make it file


buttonField : String -> String -> String -> Maybe String -> Field
buttonField name klasses text icon =
  let 
    state =
      { disabled = False }
    settings =
      { icon = icon
      , text = text
      , klasses = klasses
      , name = name
      }
  in
    SButton.init settings state |> Button


checkboxField : String -> String  -> SCheckbox.Type-> List ( Bool -> ValidationResult) -> Field
checkboxField name label type' rules =
  let
    state =
      { error = Nothing }
    settings =
      { hasField = True
      , rules = rules
      , name = name
      , label = Just label
      , displayError = True
      , type' = type'
      }
  in
    SCheckbox.init settings state False |> Checkbox

simpleDropdown : String -> String-> List (List String) -> Maybe String -> List (String -> ValidationResult) -> Field
simpleDropdown name label items key rules =
  let
    state = SDropdown.blankState
    settings = SDropdown.blankSettings
    state' =
      { state | value = key }
    settings' =
      { settings |
          name = name,
          label = label,
          items = items,
          rules = rules
      }
  in
    SDropdown.init settings' state' |> Dropdown
      
-- type alias SampleRecord =
--   { firstName : String
--   , surname : String
--   , otherNames : String
--   }

-- type alias SampleFieldSet =
--   { firstName : Field, surname : Field, otherNames : Field
--   , submit : Field, cancel: Field
--   }
  
-- type alias SampleModel = Model SampleRecord SampleFieldSet

type Field
  = None
  | Input SInput.Model
  | File SFile.Model
  | Button SButton.Model
  | Checkbox SCheckbox.Model
  | Dropdown SDropdown.Model
  

-- type alias FormField a=
--   { label: String
--   , name : String
--   , type': String
--   , getter: a-> FieldValue
--   , setter: a -> FieldValue -> a
--   , rules: List (a -> ValidationResult)
--   , isValid : Bool
--   , message : String
--   , placeholder : String
--   }

type FieldValue
  = TextValue String
  | BoolValue Bool
  | FloatValue Float
  | IntValue Int
  | NoValue


toBool : Bool -> FieldValue -> Bool
toBool default x =
  case x of
    BoolValue x -> x    
    _ -> default
 

toText : String -> FieldValue -> String 
toText default x =
  case x of
    TextValue y -> y
    IntValue y -> toString y
    FloatValue y -> toString y
    _ -> default


toInt : Int -> FieldValue -> Int
toInt default x =
  case x of
    IntValue y -> y
    TextValue y ->
      case String.toInt y of
        Result.Err msg -> default
        Result.Ok z -> z
    _ -> default

 
toFloat : Float -> FieldValue -> Float
toFloat default x =
  case x of
    FloatValue y -> y
    IntValue y -> Basics.toFloat y
    TextValue y ->
      case String.toFloat y of
        Result.Err msg -> default
        Result.Ok z -> z
    _ -> default