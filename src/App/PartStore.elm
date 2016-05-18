module App.PartStore exposing
  (Model, init)


-- IMPORTS
import App.Components.Calculator as Calculator
import App.Components.ServiceList as ServiceList
import App.Components.Firm as Firm

import App.Components.Authentication.ChangePasswordPanel as ChangePassword
import App.Components.Authentication.UserProfilePanel as UserProfile
import App.Components.Authentication.UserSettingsPanel as UserSettings

import App.Components.Lookups.IdTypesPanel as IdTypes

import App.Components.Lookups as Lookups


type alias Model =
  { serviceList : Maybe ServiceList.Model
  , calculator : Maybe Calculator.Model
  , lookups : Maybe Lookups.Model
  , firm : Maybe Firm.Model
  , changePassword : Maybe ChangePassword.Model
  , userProfile : Maybe UserProfile.Model
  , userSettings : Maybe UserSettings.Model
  , idType : Maybe IdTypes.Model
  }


init : Model
init =
  { serviceList = Nothing
  , calculator = Nothing
  , firm = Nothing
  , changePassword = Nothing
  , lookups = Nothing
  , userProfile = Nothing
  , userSettings = Nothing
  , idType = Nothing
  }