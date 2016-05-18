module App.Api exposing (..)


import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as DecodeX exposing (decode)
import Json.Encode exposing (Value, null)
import Task exposing (Task)

import App.Context as AppContext


-- url stuff
get : Maybe AppContext.Model -> String -> Decoder a -> Task never (Result String a)
get context path decoder =
  case context of
    Nothing ->
      Task.succeed <| Err "Application setup not complete. Refresh and retry"
    
    Just ctx ->
      let
        url =
          ctx.apiUrl ++ "/api/" ++ path
        task =
          HttpBuilder.get url
          |> withHeader "Authorization" ctx.authorization
          |> send stringReader stringReader
          |> Task.mapError parseError
          |> Task.map .data
      in
        task
        |> flip Task.andThen (\x ->
          case decodeString decoder x of
            Err msg ->
              Task.fail msg
            Ok d ->
              Task.succeed d
        )
        |> Task.toResult


post : Maybe AppContext.Model -> String -> Value -> Decoder a -> Task never (Result String a)
post context path jsonBody decoder =
  case context of
    Nothing ->
      noContext
    
    Just ctx ->
      let
        request =
          HttpBuilder.post (getUrl ctx path)
          |> withJsonBody jsonBody
      in
        makeRequest ctx request decoder


makeRequest : AppContext.Model -> RequestBuilder -> Decoder a -> Task never (Result String a)
makeRequest ctx request decoder =
  request
  |> withHeader "Authorization" ctx.authorization
  |> withHeader "Content-Type" "application/json;charset=UTF-8"
  |> send stringReader stringReader
  |> Task.mapError parseError
  |> Task.map .data
  |> flip Task.andThen (\x ->
    let
      x' =
        if x == "" then "\"\"" else x
    in
      case decodeString decoder x' of
        Err msg ->
          Task.fail msg
        Ok d ->
          Task.succeed d
  )
  |> Task.toResult


noContext : Task never (Result String a)
noContext =
  Task.succeed <| Err "Application setup not complete. Refresh and retry"


getUrl : AppContext.Model -> String -> String
getUrl ctx path =
  ctx.apiUrl ++ "/api/" ++ path


emptyBody : Value
emptyBody =
  null


emptyResponse : Decoder String
emptyResponse =
  Decode.succeed ""


messageResponse : Decoder String
messageResponse =
  Decode.at [ "Message" ] Decode.string


messageOrStringResponse : Decoder String
messageOrStringResponse =
  Decode.oneOf 
    [ messageResponse, Decode.string, emptyResponse ]


deleteResponse : Decoder (WithCount {})
deleteResponse =
  decode (\x -> { count = x })
  |> DecodeX.required "count" Decode.int


updateResponse : Decoder (WithCount {})
updateResponse =
  decode (\x -> { count = x })
  |> DecodeX.required "count" Decode.int


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
      case decodeString messageOrStringResponse response.data of
        Ok x ->
          x
        
        Err _ ->
          response.data


-- types
type alias WithCount a =
  { a | count : Int }