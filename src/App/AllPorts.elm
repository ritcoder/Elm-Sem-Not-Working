port module App.AllPorts exposing (..)


import App.ComponentList exposing (..)


-- messaging
type alias ToastMessage =
  { op : String
  , title : String
  , message : String
  }

port toastr : ToastMessage -> Cmd msg



-- log in/out
port logout : () -> Cmd msg
port clearUser : (Int -> msg) -> Sub msg


-- open window
newWindow : Maybe WindowConfig -> Component -> Cmd msg
newWindow window component =
  openWindow (window, toComponentConfig component)


port openWindow : (Maybe WindowConfig, ComponentConfig) -> Cmd msg
port createWindow : ((Maybe WindowConfig, ComponentConfig) -> msg) -> Sub msg