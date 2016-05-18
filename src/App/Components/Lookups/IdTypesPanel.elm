module App.Components.Lookups.IdTypesPanel exposing (..)


import Dict exposing (Dict)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeX exposing (decode)
import Json.Encode as Encode

import App.Api as Api
import App.Components.CrudBase as Base exposing (ModelBase)
import Semantic.Behaviours.FormValidation exposing (ValidationResult, notEmpty, true)
import Semantic.Elements.Button as SButton
import Semantic.Elements.Checkbox as SCheckbox
import Semantic.Elements.Input as SInput
import Semantic.Messaging as SMessaging exposing (ComponentMessage, ComponentMessages)
import Semantic.Modules.Dropdown as SDropdown
import Semantic.Views.Form as SForm

import App.Context as AppContext
import Notification exposing (info)
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { context : AppContext.Model
  , crudConfig : ModelBase Record
  , form : SForm.Model Record FieldSet
  , entityType : String
  }


init : AppContext.Model -> (Model, Cmd Msg)
init context =
  let
    crudModel = Base.defaultModel "" emptyRecord
    crudModel' =
      { crudModel |
          headers = ["Name", "Active"]
        , readCells =
            \rec ->
              [ text rec.name
              , i [ class ("large checkmark " ++ if rec.active then "green icon" else "")] []
              ]
        , deleteCells =
            \rec ->
              [ (text "Name", text rec.name)
              , (text "Notes", text rec.notes)
              , (text "Active", (text << toString) rec.active)
              ]
      }
    (crudModel'', fx) = Base.init crudModel'
    model = 
      { context = context
      , crudConfig = crudModel''
      , form = initForm ()
      , entityType = "IdType"
      }
  in
    ( model, Cmd.none )


initForm : () -> SForm.Model Record FieldSet
initForm () =
  let
    record = 
      emptyRecord
    state =
      SForm.blankState
    settings =
      { fields = fields
      , renderer = formRenderer
      , getField = getField
      , setField = setField
      , setRecordValue = updatePropertyValue
      , getRecordValue = getPropertyValue
      , fieldNames = Dict.keys fields
      }
  in
    SForm.init settings state record


emptyRecord : Record
emptyRecord =
  { id = "", name = "", notes = "", active = False }


recordDecoder : Decoder Record
recordDecoder =
  decode constructRecord
  |> DecodeX.required "id" Decode.string
  |> DecodeX.required "name" Decode.string
  |> DecodeX.optional "notes" Decode.string ""
  |> DecodeX.required "active" Decode.bool


recordEncoder : Bool -> Record -> Encode.Value
recordEncoder includeId x =
  let
    id =
      if includeId then
        [ ("id", Encode.string x.id)]
      else
        []
    fields =
      [ ("name", Encode.string x.name)
      , ("notes", Encode.string x.notes)
      , ("active", Encode.bool x.active)
      ]
  in
    id ++ fields |> Encode.object


constructRecord : String -> String -> String -> Bool -> Record
constructRecord id name notes active =
  { emptyRecord |
      id = id, name = name, notes = notes, active = active
  }


-- form related stuff
fields : FieldSet
fields =
  [ SForm.textField "name" "Name" "text" "Name" [notEmpty]
  , SForm.textField "notes" "Notes" "textarea" "Notes" []
  , SForm.checkboxField "active" "Active" SCheckbox.Toggle []
  , SForm.buttonField "createButton" "right labeled icon positive" "Save" (Just "save")
  , SForm.buttonField "updateButton" "right labeled icon primary" "Update" (Just "save")
  , SForm.buttonField "resetButton" "negative" "Reset" Nothing
  ]
  |> List.filterMap 
      (\field -> 
        case SForm.getFieldName field of
          Nothing ->
            Nothing
          
          Just name ->
            Just (name, field)
      )
  |> Dict.fromList


formRenderer (SForm.ModelRef model) =
  let
    render = SForm.renderField model
    fields = model.settings.fields
    renderField =
      \name ->
        case Dict.get name fields of
          Just field ->
            render field
          Nothing ->
            text ""
  in
    div [ class "ui form" ]
          [ h4 [ class "ui purple dividing header" ] [ text "Basic Info" ]
          , p []
            [ div [ class "fields" ]
                [ div [ class "twelve wide field" ]
                    [ renderField "name"]
                ]
            , renderField "notes"
            , renderField "active"
            ]
          , div [ class "ui right floated buttons" ]
              (
                if model.value.id == "" then
                  [ renderField "resetButton"
                  , div [ class "or" ] []
                  , renderField "createButton"
                  ]
                else
                  [ renderField "updateButton" ]
              )
      ]


getField : FieldSet -> String -> SForm.Field
getField fields name =
  fields
  |> Dict.get name
  |> Maybe.withDefault SForm.None


setField : FieldSet -> String -> SForm.Field -> FieldSet
setField fields name field =
  fields
  |> Dict.update
      name
      (\x -> Just field)


updatePropertyValue : Record -> String -> SForm.FieldValue -> Record
updatePropertyValue record name v =
  case name of
    "name" -> { record | name = SForm.toText record.name v }
    "active" -> { record | active = SForm.toBool record.active v }
    "notes" -> { record | notes = SForm.toText record.notes v }
    _ -> record


getPropertyValue : Record -> String -> SForm.FieldValue
getPropertyValue r name =
  case name of
    "name" -> SForm.TextValue r.name
    "active" -> SForm.BoolValue r.active
    "notes" -> SForm.TextValue r.notes
    _ -> SForm.NoValue


type alias Record =
  { name : String
  , active : Bool
  , notes : String
  , id : String
  }


type alias FieldSet =
  Dict String SForm.Field


name : String
name =
  "Id Types"


-- UPDATE
type Msg
  = Parent (Base.Msg Record)
  | UpdateForm (SForm.Msg Record)
  
  | Busy String
  | Idle
  | Close


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      model |> noFx |> reaction name msg


-- VIEW
view : AppContext.Model -> Model -> Html Msg
view context model =
  -- let
  --   html =
  --     case model.crudConfig.mode of
  --       -- Base.Create ->
  --       --   let
  --       --     childView =
  --       --       Html.map UpdateForm (SForm.view model.form)
  --       --   in
  --       --     Html.map Parent (Base.renderView { height = 400 } model.crudConfig childView)
  --       -- Base.Create ->
  --       --   Html.map UpdateForm (SForm.view model.form)
  --       --   |> \h -> Html.map Parent (Base.renderView { height = 400 } model.crudConfig h)
        
  --       -- Base.Update ->
  --       --   Html.map UpdateForm (SForm.view model.form)
  --       --   |> Base.renderView { height = 400 } model.crudConfig
        
  --       _ ->
  --         (Base.view { height = 400 } model.crudConfig) 
  -- in
  --   Html.map Parent html
    -- html
  div [ class "ui info icon message" ]
    [ i [ class "notched circle loading icon" ] []
    , div [ class "content" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ text "Just a moment" ]
            , p [] [ text "Men at work! Please come back later" ]
            ]
        ]
    ]


-- COMPONENT
type alias Container c =
  { c | idType : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .idType (\x m -> { m | idType = x})


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
  { icon = "list layout", title = "Identification Types", subTitle = "",  width = 450, height = 0 }