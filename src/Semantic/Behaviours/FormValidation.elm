module Semantic.Behaviours.FormValidation exposing (..)


notEmpty : String -> ValidationResult
notEmpty data =
  let
    isValid =
      data /= ""
  in
    { isValid = isValid
    , message = if isValid then "" else "This is required"
    }


true : Bool -> ValidationResult
true data =
  let
    isValid =
      data == True
  in
    { isValid = isValid
    , message = if isValid then "" else "This must be checked"
    }


valid : ValidationResult
valid =
  { isValid = True, message = ""}


-- types
type alias ValidationResult =
  { isValid : Bool
  , message : String
  }