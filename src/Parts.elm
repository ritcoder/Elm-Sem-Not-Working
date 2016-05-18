module Parts exposing
  ( embed, embedIndexed, Embedding, Observer
  , View, Update, Indexed
  , Part, instance, instance1
  , ContextualPart, ContextualView, instanceWithContext, instanceWithContext1
  , update
  , Msg
  )

{-| 

# Elm Architecture types
@docs Update, View

# Embeddings 
@docs Indexed, Embedding, embed, embedIndexed

# Part construction
@docs Msg, Part, Observer, new, new1

# Part consumption
@docs update

-}
import Dict exposing (Dict)
import Html exposing (Html)
import Html.App as Html
import Task exposing (Task)


-- TYPES


{-| Standard TEA update function type. 
-}
type alias Update model msg = 
  msg -> model -> (model, Cmd msg)


{-| Variant of TEA update function type, where effects may be 
lifted to a different type. 
-}
type alias Update' model msg msg' = 
  msg -> model -> (model, Cmd msg')



{-| Standard TEA view function type. 
-}
type alias View model msg = 
  model -> Html msg


-- EMBEDDING MODELS 


{-| Indexed families of things.
-}
type alias Indexed a = 
  Dict Int a 


{-| An __embedding__ of an Elm Architecture component is a variant in which
view and update functions know how to extract and update their model 
from a larger master model. 
-}
type alias Embedding model container msg = 
  { view : View container msg
  , update : Update container msg 
  , getModel : container -> model
  , setModel : model -> container -> container
  }
 

{-| Embed a component. Third and fourth arguments are a getter (extract the 
local model from the container) and a setter (update local model in the 
container). 

It is instructive to compare the types of the view and update function in 
the input and output:

     {- Input -}                    {- Output -}
     View model msg a            View container msg a
     Update model msg            Update container msg 

-}
embed : 
  View model msg ->               -- Given a view function, 
  Update model msg ->               -- an update function 
  (container -> model) ->              -- a getter 
  (model -> container -> container) -> -- a setter
  Embedding model container msg   -- produce an Embedding. 

embed view update get set = 
  { view = 
      \model -> view (get model)
  , update = 
      \msg model -> 
        update msg (get model)
          |> map1st (flip set model)
  , getModel = get
  , setModel = set
  }


{-| We are interested in particular embeddings where components of the same
type all have their state living inside a shared `Dict`; the individual
component has a key used to look up its own state. 
-}
embedIndexed : 
  View model msg ->                       -- Given a view function, 
  Update model msg ->                       -- an update function 
  (container -> Indexed model) ->              -- a getter 
  (Indexed model -> container -> container) -> -- a setter
  model ->                                     -- an initial model for this part
  Int ->                                       -- a part id (*)
  Embedding model container msg           -- ... produce a Part.

embedIndexed view update get set model0 id = 
  let 
    get' model = 
      Dict.get id (get model) |> Maybe.withDefault model0

    set' submodel model = 
      set (Dict.insert id submodel (get model)) model 
  in 
      embed view update get' set' 



-- LIFTING ACTIONS



{-| Similarly to how embeddings enable collecting models of different type
in a single model container, we need to collect msgs in a single "master
msg" type.  Obviously, msgs need to be eventually executed by running
the corresponding update function. To avoid this master msg type explicitly
representing the Msg/update pairs of elm-mdl components, we represent an
msg of an individual component as a partially applied update function; that
is, a function `container -> container`. E.g., the `Click` msg of Button is
conceptually represented as:

    embeddedButton : Embedding Button.Model container msg ...
    embeddedButton = 
      embedIndexed 
        Button.view Button.update .button {\m x -> {m|button=x} Button.model 0

    clickMsg : container -> container 
    clickMsg = embeddedButton.update Button.click 

When all components are embedded in the same `container` model, we 
then have a uniform update mechanism. 

We lost the ability to inspect the msg when we did this, though. To be 
able to react to some msgs of a component, we add to our `container -> 
container` type for msgs a potential __observation__ of type `obs`. 
In practice, this observation type `obs` will be the Msg of the TEA
component __hosting__ the sub-components. 

Altogether, accounting also for effects, we arrive at the following type. 
-}
type Msg container obs = 
  A (container -> (container, Cmd (Msg container obs), Maybe obs))


{-| Type of observers, i.e., functions that take an actual msg of the 
underlying TEA component to an observation.  E.g., Button has an Observer for
its `Click` msg. 
-}
type alias Observer msg obs = 
  msg -> Maybe obs


{-| Generic update function for Msg. 
-}
update : 
  (Msg container obs -> obs) ->      
  Update' container (Msg container obs) obs

update fwd (A f) container = 
  let 
    (container', fx, obs) = 
      f container
        |> map2 (Cmd.map fwd)
  in 
    case obs of 
      Nothing -> 
        (container', fx)

      Just x ->
        (container', Cmd.batch [ fx, toCmd <| Task.succeed x ])


toCmd : Task a msg -> Cmd msg
toCmd task =
  task
  |> Task.mapError (\_ -> Debug.crash "This should never happen" )
  |> Task.perform identity identity



-- INSTANCES


{-| Type of parts. A part contains a view, 
get/set/map for the inner model, and a forwarder lifting component 
msgs to observations. 
-}
type alias Part model container msg obs = 
  { view : View container obs
  , get : container -> model
  , set : model -> container -> container
  , map : (model -> model) -> container -> container
  , fwd : msg -> obs
  , update : Update' container msg obs
  }


{- TEA update function variant where running the function
produces not just a new model and an effect, but also an 
observation.
-}
type alias Step model msg obs =
  msg -> model -> (model, Cmd msg, Maybe obs)
  

{- Partially apply a step function to an msg, producing a generic Msg.
-}
pack : (Step model msg obs) -> msg -> Msg model obs
pack update msg = 
  A (update msg >> map2 (Cmd.map (pack update))) 


{- Convert an update function to a step function by applying a 
function that converts the msg input to the update function into
an observation.
-}
observe : Observer msg obs -> Update model msg -> Step model msg obs
observe f update msg =
  update msg >> (\(model', effects) -> (model', effects, f msg))


{- Return the first non-Nothing value in the list, or Nothing if no such
exists.
-}
pick : (a -> Maybe b) -> List a -> Maybe b
pick f xs = 
  case xs of 
    [] -> Nothing 
    x :: xs' -> 
      case f x of 
        Nothing -> pick f xs' 
        x -> x


{- Promote a list of Observers to a single Observer by picking, for a given
msg, the first one that succeeds. 
-}
connect : List (Observer msg obs) -> Observer msg obs
connect observers submsg = 
  pick ((|>) submsg) observers


{-| Given a lifting function, a list of observers and an embedding, construct a 
Part. 
-}
new'
  : (Msg container obs -> obs) 
  -> List (Observer msg obs) 
  -> Embedding model container msg 
  -> Part model container msg obs
new' lift observers embedding = 
  let 
    fwd = 
      pack (observe (connect observers) embedding.update) >> lift
    get = 
      embedding.getModel
    set = 
      embedding.setModel
  in
    { view = 
        \model -> 
          Html.map fwd <| embedding.view model
    , get = get
    , set = set
    , map = \f model -> set (f (get model)) model
    , fwd = fwd
    , update =
        \msg model ->
          let
            (m, fx) = 
              embedding.update msg model 
            fx' = Cmd.map fwd fx
          in
            (m, fx')
    }



{-| It is helpful to see parameter names: 

    new view update get set id lift model0 observers = 
      ...

Convert a regular Elm Architecture component (`view`, `update`) to a part, 
i.e., a component which knows how to access its model inside a generic
container model (`get`, `set`), and which dispatches generic `Msg` updates,
lifted to the consumers msg type `obs` (`lift`). You can react to msgs in
custom way by providing observers (`observers`). You must also provide an
initial model (`model0`) and an identifier for the part (`id`). The
identifier must be unique for all parts of the same type stored in the
same model (overapproximating rule of thumb: if they are in the same file,
they need distinct ids.)

Its instructive to compare the types of the input and output views:

    {- Input -}                 {- Output -}
    View model msg a         View container obs a

That is, this function fully converts a view from its own `model` and `msg`
to the master `container` model and `observation` msg. 
-}
instance
  : View model msg
  -> Update model msg
  -> (container -> Indexed model)
  -> (Indexed model -> container -> container)
  -> Int
  -> (Msg container obs -> obs)
  -> model
  -> List (Observer msg obs)
  -> Part model container msg obs

instance view update get set id lift model0 observers = 
  embedIndexed view update get set model0 id 
    |> new' lift observers


{-| Variant of `new` for parts that will be used only once in any 
TEA component. 
-}
instance1
 : View model msg
  -> Update model msg
  -> (container -> Maybe model)
  -> (Maybe model -> container -> container)
  -> (Msg container obs -> obs)
  -> model
  -> List (Observer msg obs)
  -> Part model container msg obs

instance1 view update get set lift model0 observers = 
  embed view update (get >> Maybe.withDefault model0) (Just >> set)
    |> new' lift observers 


-- with context

{-| Type of parts. A part contains a view, 
get/set/map for the inner model, and a forwarder lifting component 
msgs to observations. 
-}
type alias ContextualPart context model container msg obs = 
  { view : ContextualView context container obs
  , get : container -> model
  , set : model -> container -> container
  , map : (model -> model) -> container -> container
  , fwd : msg -> obs 
  , update : Update' container msg obs
  }


type alias ContextualView context model msg =
  context -> model  -> Html msg


type alias ContextualEmbedding context model container msg =
  { view : ContextualView context container msg
  , update : Update container msg
  , getModel : container -> model
  , setModel : model -> container -> container
  }


{-| Embed a component. Third and fourth arguments are a getter (extract the 
local model from the container) and a setter (update local model in the 
container). 

It is instructive to compare the types of the view and update function in 
the input and output:

     {- Input -}                    {- Output -}
     View model msg a            View container msg a
     Update model msg            Update container msg 

-}
embedWithContext : 
  ContextualView context model msg ->               -- Given a view function, 
  Update model msg ->               -- an update function 
  (container -> model) ->              -- a getter 
  (model -> container -> container) -> -- a setter
  ContextualEmbedding context model container msg   -- produce an Embedding. 

embedWithContext view update get set = 
  { view = 
      \ctx model -> view ctx (get model)
  , update = 
      \msg model -> 
        update msg (get model)
          |> map1st (flip set model)
  , getModel = get
  , setModel = set
  }


{-| We are interested in particular embeddings where components of the same
type all have their state living inside a shared `Dict`; the individual
component has a key used to look up its own state. 
-}
embedIndexedWithContext : 
  ContextualView context model msg ->                       -- Given a view function, 
  Update model msg ->                       -- an update function 
  (container -> Indexed model) ->              -- a getter 
  (Indexed model -> container -> container) -> -- a setter
  model ->                                     -- an initial model for this part
  Int ->                                       -- a part id (*)
  ContextualEmbedding context model container msg           -- ... produce a Part.

embedIndexedWithContext view update get set model0 id = 
  let 
    get' model = 
      Dict.get id (get model) |> Maybe.withDefault model0

    set' submodel model = 
      set (Dict.insert id submodel (get model)) model 
  in 
      embedWithContext view update get' set' 


{-| Given a lifting function, a list of observers and an embedding, construct a 
Part. 
-}
newWithContext'
  : (Msg container obs -> obs)
  -> List (Observer msg obs)
  -> ContextualEmbedding context model container msg
  -> ContextualPart context model container msg obs
newWithContext' lift observers embedding = 
  let 
    fwd = 
      pack (observe (connect observers) embedding.update) >> lift
    get = 
      embedding.getModel
    set = 
      embedding.setModel
  in
    { view = 
        \ctx model -> 
          Html.map fwd <| embedding.view ctx model
    , get = get
    , set = set
    , map = \f model -> set (f (get model)) model
    , fwd = fwd
    , update =
        \msg model ->
          let
            (m, fx) = 
              embedding.update msg model 
            fx' = Cmd.map fwd fx
          in
            (m, fx')
    }


{-| It is helpful to see parameter names: 

    new view update get set id lift model0 observers = 
      ...

Convert a regular Elm Architecture component (`view`, `update`) to a part, 
i.e., a component which knows how to access its model inside a generic
container model (`get`, `set`), and which dispatches generic `Msg` updates,
lifted to the consumers msg type `obs` (`lift`). You can react to msgs in
custom way by providing observers (`observers`). You must also provide an
initial model (`model0`) and an identifier for the part (`id`). The
identifier must be unique for all parts of the same type stored in the
same model (overapproximating rule of thumb: if they are in the same file,
they need distinct ids.)

Its instructive to compare the types of the input and output views:

    {- Input -}                 {- Output -}
    View model msg a         View container obs a

That is, this function fully converts a view from its own `model` and `msg`
to the master `container` model and `observation` msg. 
-}
instanceWithContext
  : ContextualView context model msg
  -> Update model msg
  -> (container -> Indexed model)
  -> (Indexed model -> container -> container)
  -> Int
  -> (Msg container obs -> obs)
  -> model
  -> List (Observer msg obs)
  -> ContextualPart context model container msg obs

instanceWithContext view update get set id lift model0 observers = 
  embedIndexedWithContext view update get set model0 id 
    |> newWithContext' lift observers


{-| Variant of `new` for parts that will be used only once in any 
TEA component. 
-}
instanceWithContext1
 : ContextualView context model msg
  -> Update model msg
  -> (container -> Maybe model)
  -> (Maybe model -> container -> container)
  -> (Msg container obs -> obs)
  -> model
  -> List (Observer msg obs)
  -> ContextualPart context model container msg obs

instanceWithContext1 view update get set lift model0 observers = 
  embedWithContext view update (get >> Maybe.withDefault model0) (Just >> set)
    |> newWithContext' lift observers


-- HELPERS


map2 : (b -> b') -> (a, b, c) -> (a, b', c)
map2 f (x,y,z) = (x, f y, z)


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)