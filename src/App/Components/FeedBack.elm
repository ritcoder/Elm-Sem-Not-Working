module App.Components.FeedBack exposing
  (Model, init, update
  , Part, part
  , view
  , Msg (NewContext)
  )


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.Context as AppContext exposing (User)
import Integration.Trello as Trello
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { busy: Bool
  , form : FormData
  , error : String
  , context : Maybe AppContext.Model
  }


type alias FormData =
  { type' : String
  , section : String
  , notes : String
  , name : String
  }


init : Maybe AppContext.Model -> Model
init ctx =
  { busy = False
  , form = blankForm
  , error = ""
  , context = ctx
  }


blankForm : FormData
blankForm =
  { type' = ""
  , section = ""
  , notes = ""
  , name = ""
  }


type Msg
  = SetValue String String
  | Save
  | SaveComplete (Result String String)
  | NewContext AppContext.Model


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    SetValue name value ->
      setValue name value model |> noFx
    
    Save ->
      save model
    
    SaveComplete result ->
      saveComplete result model |> noFx
    
    NewContext ctx ->
      newContext ctx model |> noFx


newContext : AppContext.Model -> Model -> Model
newContext context model =
  { model | context = Just context }


save : Model -> (Model,  Cmd Msg)
save model =
  let
    form = model.form
    typeBlank = 
      form.type' == ""
  in
  -- validate
    if form.type' == "" then
      setError "The type is required" model |> noFx
    else if form.section == "" then
      setError "The section is required" model |> noFx
    else if form.notes == "" then
      setError "The details is required" model |> noFx
    else
      case model.context of
        Nothing ->
          setError "No context found for request" model |> noFx
        
        Just context ->
          let
            model' =
              { model 
                | busy = True
                , error = ""
              }
            card =
              createCard context.user context.trello model.form
            fx =
              Trello.addCard context.trello card
              |> toCmd SaveComplete
          in
            (model', fx)


createCard : Maybe User -> Trello.Auth -> FormData -> Trello.CardInfo
createCard userInfo auth form =
  let
    name =
      form.name ++ " [" ++ form.type' ++ "] [" ++ form.section ++ "]"
    user =
      case userInfo of
        Nothing ->
          ""
        
        Just u ->
          "By " ++ "**" ++ u.name ++ "**\n" ++
          "Role " ++ "**" ++ u.role ++ "**\n\n"
    desc =
      user ++ form.notes
  in
    { name = name
    , desc = desc
    , idList = auth.listId
    }


saveComplete : Result String String -> Model -> Model
saveComplete result model =
  let
    model' =
      case result of
        Ok _ ->
          { model
            | form = blankForm
            , busy = False
            , error = ""
          }
        Err err ->
          { model
            | busy = False
            , error = err
          }
  in
    model'


setError : String -> Model -> Model
setError msg model =
  { model | error = msg }


setValue : String -> String -> Model -> Model
setValue name value model =
  let
    form = model.form
    form' =
      case name of
        "type" -> { form | type' = value }
        "section" -> { form | section = value }
        "notes" -> { form | notes = value}
        "name" -> { form | name = value }
        _ -> form
  in
    { model | form = form' }


-- VIEW setup
view : Model -> Html Msg
view model =
  div [] 
    [ createHeader model
    , createForm model
    ]


createHeader : Model -> Html Msg
createHeader model =
  div [ class " header" ]
    [ i [ class "trello icon" ] []
    , text "Feed Back"
    ]


createForm : Model -> Html Msg
createForm model =
  div 
    [ classList 
        [ ("ui form segment", True)
        , ("loading", model.busy)
        ]
    , style [ ("border-radius", "0")]
    ]
    [ div [ class "field" ]
        [ label [] [ text "Type"]
        , input' model.form "type" .type'
        ]
    , div [ class "field" ]
        [ label [] [ text "Section" ]
        , input' model.form "section" .section
        ]
    , div [ class "field" ]
        [ label [] [ text "Title" ]
        , input' model.form "name" .name
        ]
    , div [ class "field" ]
        [ label [] [ text "Details" ]
        , textarea' model.form "notes" .notes
        ]
    , div 
        [ class "ui right aligned primary button"
        , onClick Save
        ]
        [ i [ class "save icon" ] [], text "Save" ]
    
    , div 
        [ class "ui error message"
        , style [ ("display", if model.error == "" then "none" else "block") ]
        ] 
        [ text model.error]
    ]


input' : FormData -> String -> (FormData -> String) -> Html Msg
input' form name get =
  input
    [ value (get form)
    , onInput <| SetValue name
    , type' "text"
    ]
    []


textarea' : FormData -> String -> (FormData -> String) -> Html Msg
textarea' form name get =
  textarea
    [ value (get form)
    , onInput <| SetValue name
    ]
    []


-- COMPONENT
type alias Container c =
  { c | feedback : Maybe Model }


type alias Part container obs =
  Parts.Part Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instance1 view update .feedback (\x m -> { m | feedback = x})
