module App.Components.CrudBase exposing
  ( Msg (Init, NewData, OnCreateCompleted, OnDeleteCompleted, OnUpdateCompleted, NewWindow, SetRecord)
  , ModelType (Model), ModelBase
  , init, update, view, renderView
  , Mode (Create, Read, Update, Delete)
  , defaultModel
  )


-- imports
-- import Components.Common exposing (ViewConfig, ViewContext)
-- import Components.Formly as Formly exposing (FieldValue (TextValue, BoolValue), FormField, ValidationResult, toBool, toText, true, notEmpty, textAttributes, validateField)
-- import Cmd exposing (Cmd)
-- import Extras.Operators exposing (..)
-- import Extras.Encoders exposing (encodeWindowConfig)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import App.Api as Api exposing (..)
import Semantic.Messaging as SMessaging
-- import Semantic.Views.Form as SForm
-- import Task


-- model
init : ModelBase a -> (ModelBase a, Cmd (Msg a))
init model =
  -- todo: build the tasks from the permissions
  let 
    tasks = --todo: build this from permissions
      [ { icon = "teal edit", handler = \rec -> Just rec |> SetMode Update }
      , { icon = "red remove", handler = \rec -> Just rec |> SetMode Delete}
      ]
    tasks' = tasks ++ (model.tasks)
    model' =
      { model | tasks = tasks' }
  in
    (model', Cmd.none)


type alias ModelBase a =
  {
      headers : List String,
      data: List a,
      mode : Mode,
      activeRecord : a,
      emptyRecord : a,
      initialized : Bool,
      forceRefresh : Bool,
      -- formValidity : ValidationResult,
      tasks : List (ViewTask a),
      readCells : a -> List (Html (Msg a)),
      deleteCells : a -> List (Html (Msg a), Html (Msg a))
  }


type ModelType a
  = Model (ModelBase a)


type alias ViewTask a =
  { icon : String
  , handler : a -> Msg a
  }
  

defaultModel : String -> a -> ModelBase a
defaultModel entityType blank =
  { headers = []
  , data = []
  , mode = Read
  , activeRecord = blank
  , emptyRecord = blank
  , initialized = False
  , forceRefresh = False
  -- , formValidity = Formly.valid
  , tasks = []
  , readCells = \rec -> []
  , deleteCells = \rec -> []
  }


-- update
update : Msg a -> ModelBase a -> (ModelBase a, Cmd (Msg a), SMessaging.ComponentMessages)
update msg model =
  case msg of
    Init ->
      if model.initialized then 
       (model, Cmd.none, [])
      else 
        { model | initialized = True }
        |> update Refresh
    
    NewData data ->
      case data of
        Ok d ->
          ( { model | data = d }
          , Cmd.none
          , [ SMessaging.NotBusy ]
          )
        
        Err err -> --todo: alert this
          ( model, Cmd.none, [ SMessaging.NotBusy ] )
    
    Refresh ->
      ( { model | forceRefresh = False }
      , Cmd.none
      , [ SMessaging.ButtonClick "refreshButton" "refresh" [] ]
      )
      
    SetMode mode activeRecord->
      let
        -- oldRecord = model.activeRecord
        newModel =
          { model | 
              mode = mode,
              activeRecord = Maybe.withDefault model.emptyRecord activeRecord
              -- formValidity = Formly.valid
          }
        (newModel', fx, msgs) =
          case model.forceRefresh of
            True ->
              let newModel' =
                { model | forceRefresh = False }
              in
                update Refresh newModel
                
            False -> 
              ( newModel, Cmd.none, [ SMessaging.NotBusy] )
        msgs' = 
            (SMessaging.StringList "RecordChanged" []) :: msgs
      in 
        (newModel', fx, msgs')
    
    -- CreateRecord ->
    --   ( model, Cmd.none, [ SMessaging.StringList "CreateRecord" [] ] )
    
    OnCreateCompleted justRecord ->
      let newModel = 
            { model |
                forceRefresh = True,
                activeRecord =
                  case justRecord of
                    Ok _ -> 
                      model.activeRecord
                    
                    Err _ -> --todo: alert this
                      model.emptyRecord
            }
      in
        ( newModel, Cmd.none, [SMessaging.StringList "RecordChanged" [], SMessaging.NotBusy ] )          
    
    DeleteRecord -> 
      ( model, Cmd.none, [ (SMessaging.ButtonClick "deleteButton" "Delete" [] ) ] )
    
    OnDeleteCompleted _ ->
      let newModel =
            { model |
                forceRefresh = True
            }
      in
        update (SetMode Read Nothing) newModel
    
    UpdateRecord ->
      ( model, Cmd.none, [ (SMessaging.StringList "UpdateRecord" [] ) ] )
    
    OnUpdateCompleted _ ->
      let newModel =
            { model |
                forceRefresh = True
            }
      in
        update (SetMode Read Nothing) newModel
    
    SetRecord record ->
      ( { model | activeRecord = record }
      , Cmd.none
      , []
      )
    
    -- ValidateField field ->
    --   validateField model.activeRecord field
    --   |> \result ->
    --         ( model
    --         , Cmd.none
    --         , [ SMessaging.StringList "ValidateField" [field.name, toString result.isValid, result.message] ]
    --         )
     
    NewWindow title width component componentConfig ->
      ( model
      , Cmd.none
      , [ SMessaging.NewWindow { title = title, width = width } component componentConfig ]
      ) --todo: Replace with call to port


type Msg a
  = SetMode Mode (Maybe a)
  | Refresh
  | NewData (Result String (List a))
  
  -- | CreateRecord
  | OnCreateCompleted (Result String a)
  | DeleteRecord
  | OnDeleteCompleted (Result String (WithCount {}) )
  | UpdateRecord
  | OnUpdateCompleted (Result String (WithCount {}))
  -- | ValidateFields
  
  | SetRecord a
  -- | ValidateField (FormField a)
  
  | Init
  | NewWindow String Int String String


type Mode
  = Create
  | Read
  | Update
  | Delete  


-- view
view : { v | height : Int }  -> ModelBase a -> Html (Msg a)
view context model =
  let html =
    case model.mode of
      Read -> createViewForm model
      
      Delete -> createDeleteForm model
      
      _ -> div [ class "ui message" ] [ text "Cannot render anything about from Read" ]
      
  in 
    renderView context model html


renderView : { b | height : Int } -> ModelBase a -> Html (Msg a) -> Html (Msg a)
renderView context model html =
  div [] 
    [ createMenu model
    , div [ class "inner-wrapper", innerWrapper context.height ] [ html ]
    ]


createMenu : ModelBase a -> Html (Msg a)
createMenu model =
  let isActive = setActiveItem model "item"
      viewItem = a [ isActive Read ] [ i [ class "large list icon" ] [] ]
      addItem = a [ isActive Create, onClick (SetMode Create Nothing) ] [ i [ class "large add square icon" ] [] ]
      editItem = a [ isActive Update ] [ i [ class "large edit icon" ] [] ]
      deleteItem = a [ isActive Delete ] [ i [ class "large erase icon" ] [] ]
      refreshItem = a [ class "ui item", onClick Refresh ] [ i [ class "large green refresh icon" ] [] ] 
      closeItem = a [ class "ui item", onClick (SetMode Read Nothing) ] [ i [ class "large red remove icon" ] [] ]
      searchItem = div [ class "item" ]
                    [ div [ class "ui icon input" ]
                        [ input [ type' "text", placeholder "Search..." ] []
                        , i [ class "search link icon" ] []
                        ]
                    ]
      (outer, inner) =
        case model.mode of
          Create -> ([addItem], [closeItem])
          
          Read -> ([viewItem], [refreshItem, addItem])
          
          Update -> ([editItem ], [closeItem])
          
          Delete -> ([deleteItem], [closeItem])
      rightMenu = div [ class "right menu" ] inner
      menuItems = outer ++ [rightMenu]  
  in
    div [ class "ui secondary pointing menu" ] menuItems


setActiveItem : ModelBase a -> String -> Mode -> Attribute (Msg a)
setActiveItem model text mode =
  let klass = 
    if model.mode == mode
      then "active "
      else ""
  in
    klass ++ text |> class


--createTable : Signal.Address (Msg a) -> ModelBase a -> Html
-- createTable : List String ->
createTable headers tasks cells data =
  table [ class "ui celled table" ]
    [ thead []
        [ tr []
            ( (th [] [ text "" ]) ::
              ( headers
                |> List.map (\h -> th [] [ text h ])
              )
            )
        ]
    , tbody []
        ( data
          |> List.map (\record ->
                tr [ class "top aligned" ]
                  ( (td [ class "collapsing" ]
                      ( tasks
                        |> List.map
                            (\task -> i [ class (task.icon ++ " link icon"), onClick (task.handler record) ] [])
                      )
                      -- [ i [ class "teal edit link icon", onClick address (Just record |> SetMode Update) ] []
                      -- , i [ class "red remove link icon", onClick address (Just record |> SetMode Delete) ] []
                      -- ]
                    ) ::
                    ( cells record
                      |> List.map (\x -> td [] [x])
                    )
                  )
          )
        )
    ]


createViewForm : ModelBase a -> Html (Msg a)
createViewForm model =
  createTable model.headers model.tasks model.readCells model.data
  

-- updateButton : Signal.Address (Msg a) -> ModelBase a -> Html
-- updateButton address model =
--   div 
--     [ class ("ui right floated positive labelled icon submit button " ++ (if model.formValidity.isValid then "" else "disabled"))
--     , onClick address UpdateRecord
--     ] 
--     [ i [ class "save icon" ] []
--     , text "Save"
--     ]


-- createButton : Signal.Address (Msg a) -> ModelBase a -> Html
-- createButton address model =
--   div 
--     [ class ("ui right floated positive labelled icon submit button " ++ (if model.formValidity.isValid then "" else "disabled"))
--     , onClick address CreateRecord ] 
--     [ i [ class "save icon" ] []
--     , text "Save"
--     ]


-- toForm : Signal.Address (Msg a) ->ModelBase a -> List Html -> Html
-- toForm address model children =
--   ( case model.mode of
--       Create -> createButton
      
--       _ -> updateButton
--   )
--   |> \btnRenderer ->
--         div [ class "ui form" ]
--           ( children ++ [btnRenderer address model] )


createDeleteForm : ModelBase a -> Html (Msg a)
createDeleteForm model =
  div [] 
    [ div [ class "ui info icon message" ]
        [ i [ class "info circle icon" ] []
        , div [ class "content" ]
            [ div [ class "header" ]
                [ text "Delete record?" ]
            , p []
                [ text "Do you really want to delete the selected record?"
                , div [ class " ui divider" ] []
                , text "This cannot be undone"
                ]
            ]
        ]
    , table [ class "ui celled table" ]
        [ thead []
            [ tr []
                [ th [] [ text ""]
                , th [] [ text ""]
                ]
            ]
        , tbody []
            ( model.deleteCells model.activeRecord
              |> List.map (\(name, value) ->
                    tr [ class "top aligned" ]
                      [ td [] [ name ]
                      , td [] [ value ]
                      ]
              )              
            )
        , tfoot [ class "full-width" ]
            [ tr []
                [ th [] []
                , th []
                    [ div 
                        [ class "ui negative labelled icon button"
                        , onClick DeleteRecord
                        ]
                        [ i [ class "remove icon" ] []
                        , text "Delete Record"
                        ]
                    ]
                ]
            ]
        ]
    ]
  

-- styles
(=>) : a -> b -> (a, b)
(=>) = (,)


innerWrapper : Int -> Attribute (Msg a)
innerWrapper h =
  style
    [ "height" => (toString (h - 200 - 7) ++ "px")
    , "overflow-y" => "auto"
    ] 