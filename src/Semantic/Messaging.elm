module Semantic.Messaging exposing
  ( ComponentMessage (..)
  , ComponentMessages
  -- , NewWindowConfig
  )


-- imports
import Json.Decode exposing (Value)

-- functions
type ComponentMessage
  = StringList String (List String)
  | ButtonClick String String (List String)
  | TextValueChanged String String String
  | BoolValueChanged String Bool Bool
  | ValueChanged String Value
  | FileData String (List (String, Int, Value))
  | Busy String
  | NotBusy
  | FieldValidityChanged String (Maybe String)
  | NewWindow NewWindowConfig String String
  -- | CloseWindow String


type alias ComponentMessages =
  List ComponentMessage
  

type alias NewWindowConfig =
  { title: String, width: Int }