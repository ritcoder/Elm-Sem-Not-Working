module Notification exposing (..)


import App.AllPorts exposing (ToastMessage, toastr)


info : String -> String -> Cmd a
info message title =
  toastr <| ToastMessage "info" title message


warning : String -> String -> Cmd a
warning message title =
  toastr <| ToastMessage "warning" title message
  

success : String -> String -> Cmd a
success message title =
  toastr <| ToastMessage "success" title message


error : String -> String -> Cmd a
error message title =
  toastr <| ToastMessage "error" title message



