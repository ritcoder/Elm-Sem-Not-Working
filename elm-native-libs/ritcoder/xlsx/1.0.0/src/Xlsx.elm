module Xlsx
  where


import Native.Xlsx

import Task exposing (Task, fail)
import Json.Decode as Decode
import Json.Encode exposing (Value)


type Error
  = NotValidBlob
  | ReadFail
  | Error String


readFromDataUrl : String -> Task Error Value
readFromDataUrl url =
  Native.Xlsx.readFromDataUrl url


--todo: read from file 