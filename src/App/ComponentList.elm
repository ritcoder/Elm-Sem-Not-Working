module App.ComponentList exposing (..)


import Json.Encode exposing (Value, encode, object)


type Component
  = Calculator
  | ServiceList
  | Firm
  
  | UserProfile
  | UserSettings
  | ChangePassword
  
  | IdTypes
  

toComponentConfig : Component -> ComponentConfig
toComponentConfig component =
  case component of
    Calculator -> ComponentConfig "calculator" noValue
    ServiceList -> ComponentConfig "serviceList" noValue
    Firm -> ComponentConfig "firm" noValue
    UserProfile -> ComponentConfig "userProfile" noValue
    UserSettings -> ComponentConfig "userSettings" noValue
    ChangePassword -> ComponentConfig "changePassword" noValue
    IdTypes -> ComponentConfig "idTypes" noValue


toComponent : ComponentConfig -> Component
toComponent config =
  case config.name of
    "calculator" -> Calculator
    "serviceList" -> ServiceList
    "firm" -> Firm
    
    "userProfile" -> UserProfile
    "userSettings" -> UserSettings
    "changePassword" -> ChangePassword
    
    "idTypes" -> IdTypes
    
    _ -> Debug.crash <| "I do not know who to convert " ++ toString config


emptyValue : Value
emptyValue =
  object []


noValue : String
noValue =
  encode 2 emptyValue


type alias ComponentConfig =
  { name : String
  , config : String
  }


type alias WindowConfig =
  { icon : String
  , title : String
  , subTitle : String
  , width : Int
  , height : Int
  }