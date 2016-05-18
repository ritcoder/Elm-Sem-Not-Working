module Semantic.Modules.Dropdown exposing
  ( Model, init, blank, blankState, blankSettings
  , Msg (ClearError, ResetValue, Validate), update
  , view
  )


-- imports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Semantic.Behaviours.FormValidation as SValidation exposing (ValidationResult)
import Semantic.Messaging as SMessaging exposing (ComponentMessages)
import Process
import Utils exposing (..)


-- model
init : Settings -> State -> Model
init settings state =
  { settings = settings
  , state = state
  }


type alias Model =
  { settings : Settings
  , state : State
  }


blank : List DropdownItem -> Maybe Key -> Model
blank items key =
  let
    settings =
      { blankSettings | items = items }
    state =
      { blankState | value = key }
  in
    init settings state


type alias Settings =
  { items : List DropdownItem
  , placeholder : String
  , label : String
  , name : String
  , isField : Bool
  , inlineError : Bool
  , rules : List (String -> ValidationResult)
  , externalQuery : 
      Maybe 
        (String -> Int -> (Maybe (List DropdownItem) -> Msg) -> Cmd Msg )
  , localQuery :
      Maybe
        (String -> Int -> List DropdownItem)
  , templates : Templates
  , allowClear : Bool  
  }


type alias Templates =
  { item: (String, String, List String) -> List (Html Msg) }


blankSettings : Settings
blankSettings =
  { items = []
  , placeholder = ""
  , label = ""
  , name = ""
  , isField = True
  , inlineError = True
  , rules = []
  , externalQuery = Nothing
  , localQuery = Nothing
  , templates = 
      { item =
          \(k, v, rest) -> [ text v ]
      }
  , allowClear = False
  }


type alias State =
  { value : Maybe Key
  , error : Maybe String
  , status : Status
  , text : Maybe String
  , searchTerm : String
  , initialized : Bool
  , busy : Bool
  , ticks : Int
  }


blankState : State
blankState =
  { value = Nothing
  , error = Nothing
  , status = Closed
  , text = Nothing
  , searchTerm = ""
  , initialized = False
  , busy = False
  , ticks = 0
  }


type alias Key =
  String


type alias DropdownItem =
  List String


type Status
  = Open
  | Closed


name : String
name =
  "Drop down"


-- update
type alias UpdateResult =
  (Model, Cmd Msg, SMessaging.ComponentMessages)


update : Msg -> Model -> UpdateResult
update msg m =
  let
    state = m.state
    model = 
      { m | 
          state = { state | ticks = m.state.ticks + 1}
      }
  in
    case msg of
      SetValue v ->
        reaction2 name msg <| updateValue v model
      
      Validate ->
        reaction2 name msg <| validate model
      
      ClearError ->
        reaction2 name msg <| toUpdateResult <| clearError model
      
      ResetValue v ->
        reaction2 name msg <| resetValue v model
      
      ToggleStatus ->
        reaction2 name msg <| toggleStatus model
      
      SelectItem item ->
        reaction2 name msg <| toUpdateResult <| selectItem item model
        
      LoadData data ->
        reaction2 name msg <| toUpdateResult <| loadData data model
        
      RefreshData ->
        reaction2 name msg <| refreshData model
      
      SetStatus x ->
        reaction2 name msg <| setStatus x model
      
      -- DeferCloseAndValidate ->
      --   reaction2 name msg <| deferCloseAndValidate model
      
      CloseAndValidate ->
        reaction2 name msg <| closeAndValidate model
      
      ExecuteAfter delay act ->
        reaction2 name msg <| executeAfter delay act model


executeAfter : Float -> Msg -> Model -> UpdateResult
executeAfter delay msg model =
  let
    fx =
      Process.sleep delay
      |> toCmd (always msg)
  in
    (model, fx, [])


-- deferCloseAndValidate : Model -> UpdateResult
-- deferCloseAndValidate model =
--   let
--     fx =
--       Task.sleep 500
--       |> Task.toMaybe
--       |> Task.map (\_ -> CloseAndValidate)
--       |> Cmd.task
--   in
--     (model, fx, [])


closeAndValidate : Model -> UpdateResult
closeAndValidate model =
  let
    (model', fx, msgs) =
      setStatus Closed model
    (model'', fx2, msgs2) =
      validate model'
  in
    (model'', Cmd.batch [fx, fx2], msgs ++ msgs2)


setStatus : Status  -> Model -> UpdateResult
setStatus x model =
  case model.state.status == x of
    True -> 
      toUpdateResult model
    False ->
      toggleStatus model


selectItem : (String, String, List String) -> Model -> Model
selectItem (k, v, rest) model =
  let
    state =
      model.state
    state' =
      { state |
          value = Just k,
          text = Just v
      }
  in
    { model | state = state' }


toggleStatus : Model -> UpdateResult
toggleStatus model =
  let
    (model', fx, msgs ) =
      case model.state.initialized of
        False ->
          refreshData model        
        True ->
          toUpdateResult model
    model'' =
        let
          state = model'.state
          status =
            case state.status of
              Open -> Closed
              Closed -> Open
          state' = 
            { state | status = status }
        in
          { model' | state = state' }
  in
    (model'', fx, msgs)


refreshData : Model -> UpdateResult
refreshData model =
  let
    state = model.state
    settings = model.settings
    (state', fx, items) =
      case (model.settings.localQuery, model.settings.externalQuery) of
        (Just q, _) ->
          ( { state | 
              initialized = True,
              busy = False
            }
          , Cmd.none
          , q state.searchTerm 0  |> Just
          )
        (_, Just q) ->
          ( { state |
                busy = True,
                initialized = True
            }
          , q state.searchTerm 0 LoadData
          , Nothing
          )
        _ ->
          ( { state |
                initialized = True,
                busy = False
            }
          , Cmd.none
          , Just (model.settings.items)
          )
    model' =
      { model | state = state' }
    (model'', fx2, msgs) =
      case items of
        Nothing -> (model', Cmd.none, [])
        Just data ->
          update (LoadData items) model'
  in
    ( model''
    , Cmd.batch [ fx, fx2 ]
    , msgs
    )


loadData : Maybe (List DropdownItem) -> Model -> Model
loadData data model =
  let
    state = model.state
    settings = model.settings
    items =
      case data of
        Nothing -> settings.items
        Just d -> d
    state' =
      { state |
          initialized = True,
          busy = False
      }
    settings' =
      { settings | items = items }
    model' =
      { model | 
          state = state', 
          settings = settings'
      }
    (model'', _, _) =
      setTextFromValue model'
  in
    model''
      

updateValue : Maybe Key -> Model -> UpdateResult
updateValue k model =
  let
    state = model.state
    msgs =
      case state.value == k of
        True -> []
        False ->
          let
            (nv, ov) =
              case (k, state.value) of
                (Just a, Just b ) -> (a, b)
                (Just a, Nothing) -> (a, "")
                (Nothing, Just b) -> ("", b)
                (Nothing, Nothing) -> ("", "")
          in
            [ SMessaging.TextValueChanged model.settings.name nv ov ]
    state' =
      { state |
          value = k,
          text = Nothing --todo: set this from somewhere
      }
    model' =
      { model | state = state' }
    (model'', fx, msgs2) =
      setTextFromValue model'
  in
    (model'', fx, msgs ++ msgs2)


setTextFromValue : Model -> UpdateResult
setTextFromValue model =
  case (model.state.value, model.state.text) of
    (Just key, Nothing) -> 
      case model.state.initialized of
        False ->
          refreshData model
        True ->    
          let
            state = model.state
            -- key =
            --   case model.state.value of
            --     Nothing -> ""
            --     Just x -> x
            text =
              model.settings.items
              |> List.map
                  (\xs ->
                      case xs of
                        a :: b :: r -> (a, b)
                        a :: []-> (a, a)
                        [] -> ("-", "")
                  )
              |> List.filter
                  (\(k,t) -> k == key )
              |> List.head
              |> Maybe.map (\(k,t) -> t)
            state' =
              { state | text = text }
            model' =
              { model | state = state' }
          in
            toUpdateResult model'
    (Nothing, _) -> 
      toUpdateResult model
    (_, Just _) ->
      toUpdateResult model


validate : Model -> UpdateResult
validate model =
  let
    state = 
      model.state
    value =
      case state.value of
        Nothing -> ""
        Just x -> x
    state' =
      model.settings.rules
      |> List.map (\rule -> rule value)
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


updateAndValidate : Maybe String -> Model -> UpdateResult
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


clearError : Model -> Model
clearError model =
  let
    state = model.state
    state' =
      { state | error = Nothing }
  in
    { model | state = state'}


resetValue : Maybe Key -> Model -> UpdateResult
resetValue k model =
  let
    key =
      case k of
        Just "" -> Nothing
        Nothing -> Nothing
        Just x -> k
    (m, fx, msgs) =
      updateValue key model
    (m', fx2, msgs2) =
      clearError m |> toUpdateResult
  in
    (m', Cmd.batch [fx, fx2], msgs ++ msgs2)


toUpdateResult : Model -> UpdateResult
toUpdateResult model =
  ( model, Cmd.none, [] )
  

type Msg
  = SetValue (Maybe String)
  | ResetValue (Maybe String)
  | Validate
  | ClearError
  | ToggleStatus
  | SelectItem (String, String, List String)
  | LoadData (Maybe (List DropdownItem))
  | RefreshData
  | SetStatus Status
  -- | DeferCloseAndValidate
  | CloseAndValidate
  | ExecuteAfter Float Msg


-- view
view : Model -> Html Msg
view model =
  case model.settings.isField of
    False -> 
      ui model
    
    True ->
      div [ fieldClass model ]
        ( case (model.settings.inlineError, model.state.error) of
            (True, Just errorMsg) ->
              [ label [] [ text model.settings.label ]
              , ui model
              , div [ class "ui basic red pointing prompt label" ] [ text errorMsg ]
              ]
            _ -> 
              [  label [] [ text model.settings.label ]
              , ui model
              ]
        )


ui : Model -> Html Msg
ui model =
    div 
      [ dropdownClass model.state
      , onClick (if model.state.status == Open then SetStatus Closed else SetStatus Open )
      , tabindex 0
      , onFocus (ExecuteAfter 250 (SetStatus Open))
      -- , onBlur address (SetStatus Closed)
      , onBlur (ExecuteAfter 300 CloseAndValidate)
      ]
      [ input [ type' "hidden" ] []
      , i 
          [ class "dropdown icon"
          ] 
          []
      , div [ class ("text " ++ if isNothing model.state.text then "default" else "") ] 
          [  text <| Maybe.withDefault model.settings.placeholder model.state.text ]
      , div 
          [ menuClass model.state
          , tabindex -1
          ]
          ( model.settings.items
            |> List.map
                (\item ->
                    case item of
                      k :: [] -> (k, k, [])                            
                      k :: v :: x -> (k, v, x)                          
                      _ -> ("", "", [])                      
                )
            |> List.filter
                (\(k,v,r) -> k /= "")
            |> List.map
                (\(k, v, r) ->
                    div 
                      [ itemClass model (k, v, r)
                      , onClick (SelectItem (k, v, r))
                      ] 
                      ( model.settings.templates.item (k, v, r) )
                )
          )
      ]


itemClass : Model -> (String, String, List String) -> Attribute Msg
itemClass model (k, v, r) =
  let
    selected =
      case model.state.value of
        Nothing -> ""
        Just x ->
          if x == k then
            "active selected"
          else
            ""
  in
    "item " ++ selected |> class


dropdownClass : State -> Attribute Msg
dropdownClass state =
  let
    others =
      case state.status of 
        Open -> " visible active"
        Closed -> ""
    loading =
      case state.busy of
        True -> " loading"
        False -> ""
  in
    "ui dropdown selection" ++ others ++ loading |> class


menuClass : State -> Attribute Msg
menuClass state =
  let
    others =
      case state.status of
        Open -> " visible"
        Closed -> " hidden"
  in
    "menu transition" ++ others |> class


fieldClass : Model -> Attribute Msg
fieldClass model =
  let
    baseClass = "field"
    error =
      case model.state.error of
        Just x -> " error"
        Nothing -> ""
  in
    baseClass ++ error |> class


-- helpers
isNothing : Maybe a -> Bool
isNothing x =
  case x of
    Nothing -> True
    _ -> False
