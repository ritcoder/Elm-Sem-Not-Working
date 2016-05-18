module App.Context exposing
  ( Model, User, AppInfo
  , init
  )


import App.Assets as Assets
import Integration.Trello as Trello


-- model
type alias Model =
  { apiUrl : String
  , user : Maybe User
  , appInfo : AppInfo
  , trello : Trello.Auth
  , authorization : String
  }


type alias User =
  { userName : String
  , pic : String
  , role : String
  , name : String
  }
  
  
type alias AppInfo =
  { name : String
  , version : String
  , smallLogo : String
  }

  
init : String -> AppInfo -> Trello.Auth -> Maybe User -> String -> Model
init url appInfo trello user authorization =
  { apiUrl = url
  , user = user
  , appInfo = appInfo
  , trello = trello
  , authorization = authorization
  }