module App.Components.Calculator exposing
  ( Model, init, update
  , Part, part
  , view
  , partView
  )


-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

import App.Context as AppContext
import Parts
import Utils exposing (..)


-- MODEL
type alias Model =
  { basicButtons : List ( List Button)
  , opButtons : List Button
  , memoryButtons : List Button  
  , calculation : String
  , current : String
  , operator : Maybe String
  , accumulator : Float
  , requireClear : Bool
  , lastButton : Maybe ButtonType
  , memory : Maybe Float
  }


type alias Button =
  ( String, ButtonType, Bool )


type ButtonType
  = MemoryClear
  | MemoryRecall
  | MemoryPlus
  | MemoryMinus
  | MemoryStore
  
  | Function FunctionType
  
  | CE
  | C
  | Back
  
  | Op String
  | Equals
  | Digit Int
  
  | Dot
  | PlusMinus


type FunctionType
  = Percent
  | Sqrt
  | Sqr
  | Inv


init : Model
init =
  { basicButtons = 
      [ [ ("CE", CE, True)
        , ("C", C, True)
        , ("Del", Back, True)
        , ("/", Op "/", True)
        ]
      , [ ("7", Digit 7, True)
        , ("8", Digit 8, True)
        , ("9", Digit 9, True)
        , ("*", Op "*", True)
        ]
      , [ ("4", Digit 4, True)
        , ("5", Digit 5, True)
        , ("6", Digit 6, True)
        , ("-", Op "-", True)
        ]
      , [ ("1", Digit 1, True)
        , ("2", Digit 2, True)
        , ("3", Digit 3, True)
        , ("+", Op "+", True)
        ]
      , [ ("+/-", PlusMinus, True)
        , ("0", Digit 0, True)
        , (".", Dot, True)
        , ("=", Equals, True)
        ]
      ]
  , opButtons = 
      [ ("%", Function Percent, True)
      , ("Sqrt", Function Sqrt, True)
      , ("Sqr", Function Sqr, True)
      , ("Inv", Function Inv, True)
      ]
  , memoryButtons = 
      [ ("MC", MemoryClear, True )
      , ("MR", MemoryRecall, True)
      , ("M+", MemoryPlus, True)
      , ("M-", MemoryMinus, True)
      , ("MS", MemoryStore, True)
      ]
  , calculation = ""
  , current = "0"
  , accumulator = 0.0
  , operator = Nothing
  , requireClear = False
  , lastButton = Nothing
  , memory = Nothing
  }


maxLength : Int
maxLength = 30


-- ACTION, UPDATE
type Msg
  = ButtonMsg ButtonType


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ButtonMsg t ->
      let
        (model', fx) =
          case t of
            Digit x ->
              addDigit x model |> noFx
            
            Dot ->
              dot model |> noFx
            
            PlusMinus ->
              plusMinus model |> noFx
            
            CE ->
              clear False model |> noFx
            
            C ->
              clear True model |> noFx
              
            Op op ->
              operate op model |> noFx
            
            Equals ->
              equals model |> noFx
            
            Function Sqrt ->
              sqrt model |> noFx
            
            Function Sqr ->
              sqr model |> noFx
            
            Function Inv ->
              inv model |> noFx
            
            Function Percent ->
              percent model |> noFx
             
            MemoryClear ->
              { model | memory = Nothing } |> noFx
            
            MemoryRecall ->
              memoryRecall model |> noFx
            
            MemoryPlus ->
              memoryPlus model |> noFx
            
            MemoryMinus ->
              memoryMinus model |> noFx
            
            MemoryStore ->
              memoryStore model |> noFx
            
            Back ->
              back model |> noFx
              
        model'' =
            { model' | lastButton = Just t }
        in
          (model'', fx)


back : Model -> Model
back model =
  let
    current = 
      String.dropRight 1 model.current
    current' =
      case current of
        "" ->
          "0"
          
        "-" ->
          "0"
        
        _ ->
          current
    model' =
      { model | current = current' }
  in
    model'


memoryPlus : Model -> Model
memoryPlus model =
  let
    memory =
      case model.memory of
        Just x ->
          x + model.accumulator
        
        Nothing ->
          model.accumulator
  in
    { model | memory = Just memory }


memoryRecall : Model -> Model
memoryRecall model =
  let
    memory =
      case model.memory of
        Just x ->
          x
        
        Nothing ->
          0
  in
    { model | current = toString memory }


memoryMinus : Model -> Model
memoryMinus model =
  let
    memory =
      case model.memory of
        Just x ->
          x - model.accumulator
        
        Nothing ->
          model.accumulator
  in
    { model | memory = Just memory }


memoryStore : Model -> Model
memoryStore model =
  { model | memory = Just model.accumulator }


percent : Model -> Model
percent model =
  let
    current = eval model.current
    evaluated = 
      case model.accumulator of
        0 ->
          0.0
          
        x ->
          current * x / 100
    model' =
      equals { model | current = toString evaluated }
  in
    model'
    

inv : Model -> Model
inv model =
  let
    current = eval model.current
    model' =
      case current of
        0 ->
          setError model "Cannot divide by zero"
        
        _ ->
          let
            evaluated =
              1 / current
          in
            equals  { model | current = toString evaluated }
  in
    model'


sqr : Model -> Model
sqr model =
  let
    current = eval model.current
    evaluated = current * current
    model' =
      equals { model | current = toString evaluated }
  in
    model'
   

sqrt : Model -> Model
sqrt model =
  let
    evaluated = eval model.current 
    model' =
      case evaluated < 0 of
        True ->
          setError model "Cannot find square root of -ve number"
        
        False ->
          equals
            { model
              | current = evaluated |> Basics.sqrt |> toString
            }
  in
    model'
   

equals : Model -> Model
equals model =
  let
    model' =
      operate "" model
  in
    { model' | operator = Nothing }


operate : String -> Model -> Model
operate op model =
  let
    (hasOperator, prevOperator) =
      case model.operator of
        Nothing -> 
          (False, "")        
        Just x -> 
          (True, x)
    
    changeOperator =
      case (hasOperator, model.lastButton) of
        (True, Just (Op _)) ->
          True
        
        (_, Just Equals) ->
          True
        
        (_, Just (Function _)) ->
          True
        
        _ ->
          False
          
    calculation =
      ( if changeOperator then
          String.dropRight (String.length prevOperator) model.calculation
          ++ " " ++ op
        else
          case prevOperator of
            "" ->
              model.current ++ " " ++ op
            
            _ ->
              model.calculation ++ " " ++ model.current ++ " " ++ op
      )
    
    (error, accumulator) =
      if changeOperator then
        ("", model.accumulator)
      else
        case applyOperator prevOperator model.accumulator (eval model.current) of
          Ok x ->
            ("", x)
          Err x ->
            (x, model.accumulator)
    
  in
    { model
      | operator = Just op
      , calculation = calculation
      , current = if error == "" then "0" else error
      , operator = Just op
      , accumulator = accumulator
    }


{-- Clear the stores. If all is true, clear the everything --}
clear : Bool -> Model -> Model
clear all model =
  if all then
    { model
      | current = "0"
      , operator = Nothing
      , accumulator = 0.0
      , calculation = ""
    }
  else
    { model
      | current = "0"
    }


{--add a digit ensure the max length is not exceeded --}
addDigit : Int -> Model -> Model
addDigit d m =
  let
    model =
      case m.lastButton of
        Just Equals -> 
          clear True m
        
        _ ->
          m
  in
    if String.length model.current > maxLength then
      { model 
        | current = "Number too long"
        , requireClear = True
      }
    else
      if (eval model.current == 0) &&  not (String.contains "." model.current) then
        { model | current = toString d }
      else
        { model | current = model.current ++ (toString d) }


{-- Insert the . when appropriate --}
dot : Model -> Model
dot model =
  if String.length model.current == 0 then
      { model | current = "0." }
  else
    if not <| String.contains "." model.current then
      { model | current = model.current ++ "." }
    else
      model


{-- change the sign --}
plusMinus : Model -> Model
plusMinus model =
  if String.contains "e" model.current then
    let
      ePos = 
        indexOf "e-" model.current
      current' =
        if ePos /= -1 then
          model.current
          |> String.split "e-"
          |> String.join "e"
        else
          model.current
          |> String.split "e"
          |> String.join "e-"
    in
      { model | current = current' }
  else
    if String.startsWith "-" model.current then
      { model | current = String.dropLeft 1 model.current }
    else
      { model | current = "-" ++ model.current }
    
        
applyOperator : String -> Float -> Float -> Result String Float
applyOperator op x y =
  case op of
    "+" ->
      Ok <| x + y
    
    "-" ->
      Ok <| x - y
    
    "*" ->
      Ok <| x * y
    
    "/" ->
      case y of
        0.0 ->
          Err "Cannot divide by 0"
        
        _ ->
          Ok <| x / y
    
    "" ->
      Ok y
    
    _ ->
      Err <| "Not sure how to process " ++ op


setError : Model -> String -> Model
setError model error =
  { model
    | current = error
    , calculation = ""
  }
  

eval : String -> Float
eval x =
  case String.toFloat x of
    Ok x' ->
      x'
    
    Err _ ->
      0.0


indexOf : String -> String -> Int
indexOf sub str =
  case String.indices sub str of
    [] ->
      -1
    
    a::b ->
      a


-- VIEW setup
view : Model -> Html Msg
view model =
  div 
    []
    [ createHeader model
    , div [ style [ ("margin-bottom", "0.5rem") ] ] []
    , createLabels model
    , createMemoryUi model
    , div 
        [ class "ui divider"
        , style [ ("margin-top", "0"), ("margin-bottom", "0") ]
        ]
        []
    , createOpUi model
    , createBasicOpUi model
    ]


createHeader : Model -> Html Msg
createHeader model =
  div [ class "header" ]
    [ i [ class "calculator icon" ] []
    , text "Calculator"
    ]


createLabels : Model -> Html Msg
createLabels model =
  div []
    [ div 
        [ class "ui huge basic label"
        , style 
            [ ("width", "100%")
            , ("text-align", "right")
            , ("border-radius", "0")
            , ("border-bottom-width", "0")
            ]
        ]
        [ text <| toString model.accumulator ]
    , br [] []
    , div 
        [ class "ui basic label"
        , style 
            [ ("width", "100%")
            , ("text-align", "right")
            , ("border-radius", "0")
            , ("border-top-width", "0")
            , ("border-bottom-width", "0")
            , ("margin-left", "0")
            ]
        ]
        [ text model.calculation ]      
    , br [] []
    , div 
        [ class "ui pointing below basic label"
        , style 
            [ ("width", "100%")
            , ("text-align", "right")
            , ("border-radius", "0")
            , ("border-top-width", "0")
            , ("margin-left", "0")
            ]
        ]
        [ text model.current ]      
    ]


createMemoryUi : Model -> Html Msg
createMemoryUi model =
  div 
    [ class "ui small fluid buttons"
    ]
    ( model.memoryButtons
      |> List.map renderButton
    )


createOpUi : Model -> Html Msg
createOpUi model =
  div 
    [ class "ui small fluid buttons" ]
    ( model.opButtons
      |> List.map renderButton
    )


createBasicOpUi : Model -> Html Msg
createBasicOpUi model =
  div 
    [ style [ ("background-color", "lavenderblush") ] ]
    ( model.basicButtons
      |> List.map 
          (\xs ->
             [div
              [ class "ui small fluid buttons" ]
              ( xs
                |> List.map renderButton
              )
              , br [] [] ]
          )
      |> List.concat
    )


renderButton : (String, ButtonType, Bool) -> Html Msg
renderButton (txt, btnType, active) =
      div 
        [ class "ui basic button"
        , style [ ("border-radius ", "0px") ]
        , onClick (ButtonMsg btnType)
        ]
        [ text txt]


-- COMPONENT
type alias Container c =
  { c | calculator : Maybe Model }


type alias Part container obs =
  Parts.Part Model container Msg obs


part : 
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instance1 view update .calculator (\x m -> { m | calculator = x })


observe : 
  { b | busy : String -> a, close: a, idle: a }
  -> Msg
  -> Maybe a
observe observer msg =
  Nothing


partView :
  AppContext.Model
  -> (Parts.Msg (Container a) b -> b)
  -> (Parts.ContextualView AppContext.Model (Container a) b -> d)
  -> { e | busy : String -> b, close: b, idle: b }
  -> ( d, Cmd b )
partView context msgTagger toPartView observer =
  let
    instance =
      part msgTagger init [ observe observer ]
  in 
    (toPartView <| \ctx m -> instance.view m, Cmd.none)