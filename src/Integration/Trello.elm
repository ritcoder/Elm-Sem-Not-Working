module Integration.Trello exposing
  ( Auth, addCard
  , addCard2, post
  , CardInfo
  )


-- IMPORTS
import HttpBuilder exposing (..)
import Task exposing (Task)



-- MODEL
baseUrl : String
baseUrl = "https://api.trello.com/1/"

type alias Auth =
  { token : String
  , appKey : String
  , boardId : String
  , listId : String
  }


type alias CardInfo =
  { name : String
  , desc : String
  , idList : String
  }


cardAsStringList : CardInfo -> List (String ,String)
cardAsStringList card =
  [ ("name", card.name)
  , ("desc", card.desc)
  , ("idList", card.idList)
  ]

-- FUNCTIONS

addCard : Auth -> CardInfo -> Task a (Result String String)
addCard auth cardInfo =
  post (buildUrl auth "cards") (cardAsStringList cardInfo)


addCard2 : Auth -> String -> String -> String -> Task a (Result String String)
addCard2 auth listId name description =
  let
    card =
      { name = name
      , desc = description
      , idList = listId
      }
  in
    addCard auth card


post : String -> List (String, String) -> Task never (Result String String)
post url content =
  HttpBuilder.post url
  |> withMultipartStringBody content
  |> send stringReader stringReader
  |> Task.mapError parseError
  |> Task.map .data
  |> Task.toResult


parseError : Error String -> String
parseError err =
  case err of
    NetworkError ->
      "Network error"
    
    Timeout ->
      "Request timed out"
    
    UnexpectedPayload payload ->
      "Invalid request " ++ payload
    
    BadResponse response ->
      response.data


buildUrl : Auth -> String -> String
buildUrl auth url =
  baseUrl ++ url ++ "?key=" ++ auth.appKey ++ "&token=" ++auth.token