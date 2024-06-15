{-# LANGUAGE OverloadedStrings #-}
module CL.Define
  ( AppModel(..)
  , AppEvent(..)
  ) where

import CL.Define.Core
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.Define.Sandbox

data AppModel = AppModel
  { _appModelCataclysmVariant :: CataclysmVariant
  , _appModelSelectedTab :: SelectTab
  , _appModelIsLoading :: Bool
  , _appModelGame :: GameModel
  , _appModelMods :: ModsModel
  , _appModelSoundpacks :: SoundpacksModel
  , _appModelFonts :: FontsModel
  , _appModelBackups :: BackupsModel
  , _appModelSettings :: SettingsModel
  , _appModelSandbox :: SandboxModel
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSelectTab SelectTab
  | AppSandbox SandboxEvent
  | AppGame GameEvent
  | AppMods ModsEvent
  | AppSoundpacks SoundpacksEvent
  | AppFonts FontsEvent
  | AppBackups BackupsEvent
  | AppSettings SettingsEvent
  deriving (Eq, Show)
