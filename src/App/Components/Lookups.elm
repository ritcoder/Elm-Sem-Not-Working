module App.Components.Lookups exposing (..)




import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.Context as AppContext
import Parts



type alias Model =
  { text : String }


init =
  { text = "" }


type Msg
  = Set String


update msg model =
  case msg of
    Set text ->
      ({model | text = text}, Cmd.none)


view context model =
  div [ class "ui message" ]
    [ div [ class "ui dividing header" ] [ text "Hello"]
    , div []
        [ input 
            [ type' "text"
            , placeholder "value"
            , value model.text
            , onInput Set
            ]
            []
        , text model.text
        ]
    ]


-- COMPONENT
type alias Container c =
  { c | lookups : Maybe Model }


type alias Part container obs =
  Parts.ContextualPart AppContext.Model Model container Msg obs


part :
  (Parts.Msg (Container c) obs -> obs)
  -> Model
  -> List (Parts.Observer Msg obs)
  -> Part (Container c) obs
part =
  Parts.instanceWithContext1 view update .lookups (\x m -> { m | lookups = x})

