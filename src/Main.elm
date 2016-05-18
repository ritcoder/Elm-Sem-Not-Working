import Html.App as Html

import App.Instance as App
import App.Context as Context exposing (AppInfo, User)
import App.Assets as Assets exposing (smallLogo)
import Integration.Trello as Trello


main : Program StartInfo
main =
    Html.programWithFlags
      { init = initWithFlags
      , view = App.view
      , update = App.update
      , subscriptions = App.subscriptions
      }


initWithFlags : StartInfo -> (App.Model, Cmd App.Msg)
initWithFlags startInfo =
  let
    context =
      Context.init startInfo.serverUrl appInfo trello user authorization
  in
    App.init context


type alias StartInfo =
  { serverUrl : String }


authorization : String --this will come after login
authorization =
  "Bearer fuw9NcTlluVcVGqGaApTNQfRx35Vq6-t4Kn54v5Ahoy-GFsouTSugmdaRVlZfGiXJaab8vYRAkW4uv10NAUjFJ_dcxkwzcgfb3LXUkEpxY8KEcT2Q28qpchSYouDKUUMl2RJxKF1s1xpDSrdfYtys70ITLGC-fYf_2JcHtOOFEuvi9FamUdD7kiapQMqiDEG-kfqGDq5XFuBrvdn8P0cU6cV3Y6lrIdGmWGosjJNJ8wWqERDnX4UXtTj3OSt37Io69UcLRhD33m5oxKC744Lz3XZyAr3OoreBId3oj3WQte0rQ9_Szt1axCK5wmYSBuEz7rxOnEhikOSVA2LRlf-UjwKLxOKxFk2UQ8H-NpKla8n-A1t01HR2SL7uF8JOSPNPl0_DZIJXKnf_rtiJ9FYc47Jm7Ttu6umQDBqnc3RJC-xXlN1-SVN9vg49_DzunRlbJqP6oL3Liu25db7izWHjI3Ljrl63nduxEtvHDDqvghZ6SWqsYxQctcnxCgnlFORjoFozCb8J4uF7gyQVFBs5XXZ_CJXct__Wtjlp58F-jwOQzuSxetFy8-HJ6xZsKp4"


appInfo : AppInfo
appInfo =
  { name = "App Title here"
  , version = "0.1a"
  , smallLogo = smallLogo
  }


trello : Trello.Auth
trello =
  { token = "token"
  , appKey = "app-key"
  , boardId = "board-id"
  , listId = "list-id"
  }


user : Maybe User
user =
  if False then Nothing else 
  Just
    { userName = "Richard"
    , pic = Assets.image "user_pic.jpg"
    , role = "Application Tester"
    , name = "Richard Osafo"
    }