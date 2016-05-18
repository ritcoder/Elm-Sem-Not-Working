module Semantic.Elements.File exposing
  ( Model, init
  , blank, blankSettings, blankState
  , Msg (ClearError, ResetValue, Validate), update
  , view
  )


-- imports
import FileReader exposing (NativeFile)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Value)
-- import Json.Encode as Encode exposing (encode)
-- import Semantic.Behaviours.FormValidation exposing (ValidationResult, valid)
import Semantic.Messaging as Messaging exposing (ComponentMessages)
import Task exposing (Task)


-- types
type alias Model =
  { value : FileValue
  , settings : Settings
  , state : State
  }


type alias State =
  { error : Maybe String
  , files : List FileData
  , fileCount : Int
  }


type alias Settings =
  { hasField : Bool
  , placeholder : String
  , label : Maybe String
  , required : Bool
  , multiple : Bool
  , name : String
  , displayError : Bool
  , fileParser : FileParser
  }


type alias FileData =
  ( String, Int, Value )


type FileValue
  = NoValue
  | Files (List FileData)


type FileParser
  = DataUrl
  | XlsxJson
  | BinaryString
  | CustomParser (FileData -> Task String FileData)


type Msg
  = SetValue String
  | ResetValue String
  | Validate
  | ClearError    
  | ParseFiles (List NativeFile)
  | TransmitFiles (Result String (List FileData))


-- model
init : Settings -> State -> Model
init settings state =
  { value = NoValue
  , settings = settings
  , state = state
  }



blank : Model
blank =
  init blankSettings blankState


blankSettings : Settings
blankSettings =
  { hasField = True
  , placeholder =""
  , label = Just ""
  , required = False
  , multiple = False
  , name = ""
  , displayError = True
  , fileParser = DataUrl
  }


blankState : State
blankState =
  { error = Nothing
  , files = []
  , fileCount = -1
  }


-- update
update : Msg -> Model -> (Model, Cmd Msg, ComponentMessages)
update action model =
  (model, Cmd.none, [])


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
      

onFileChange : (List NativeFile -> Msg) -> Attribute Msg
onFileChange tagger =
  on "change" (Decode.map tagger FileReader.parseSelectedFiles)

 
createInput : Model -> Html Msg
createInput model =
  div [ class "ui segment" ]
    [ label []
        [ input 
            [ type' "file"
            , multiple model.settings.multiple
            , onFileChange ParseFiles
            , onBlur Validate
            ] 
            []
        ]
    ]


klass : Model -> Attribute Msg
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