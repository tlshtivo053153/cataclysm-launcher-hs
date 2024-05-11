module CL.Define.Settings
  ( SettingsModel(..)
  , SettingsEvent(..)
  ) where

data SettingsModel = SettingsModel
  { _settingsModelAnysetting :: Int }
  deriving (Eq, Show)

data SettingsEvent = SettingsEvent
  deriving (Eq, Show)
