module Utils exposing (..)


import Task exposing (Task, mapError)


-- Update utils
updatePart : (partMsg -> model -> (model, Cmd msg)) -> partMsg -> (model,Cmd msg) -> (model, Cmd msg)
updatePart updateFn message (model,fx) =
  updateFn message model
  |> \(model', fx1) ->
        (model', Cmd.batch [fx, fx1])


updatePart2 : (partMsg -> model -> (model, Cmd msg)) -> partMsg -> model -> (model, Cmd msg)
updatePart2 updateFn message model =
  updatePart updateFn message (model, Cmd.none)


reaction : String -> msg -> (model, Cmd msg) -> (model, Cmd msg)
reaction source msg result =
  let
    _ = Debug.log source msg
  in
    result


reaction2 : String -> msg -> a -> a
reaction2 source msg result =
  let
    _ = Debug.log source msg
  in
    result


-- Cmd utils
noFx : a -> (a, Cmd b)
noFx x =
  (x, Cmd.none)


toCmd : (a -> msg) -> Task never a -> Cmd msg
toCmd tagger task =
  task
  |> Task.mapError (failWith "This should never happen" )
  |> Task.perform identity tagger


toCmd2 : msg -> Cmd msg
toCmd2 msg =
  let
    task = Task.succeed 0
    tagger = \_ -> msg
  in
    toCmd tagger task



failWith : String -> a -> b
failWith msg  =
  \_ -> Debug.crash msg
