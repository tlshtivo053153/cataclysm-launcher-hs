{-# LANGUAGE OverloadedStrings #-}
module CL.Define
  ( CataclysmVariant(..)
  , SelectTab(..)
  , AppModel(..)
  , AppEvent(..)
  ) where

import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.Define.Sandbox

data CataclysmVariant
  = CDDA
  | CBN
  deriving (Eq)

data SelectTab
  = TabSandbox
  | TabGame
  | TabMods
  | TabSoundpacks
  | TabFonts
  | TabBackups
  | TabSettings
  deriving (Eq)

instance Show SelectTab where
  show TabSandbox = "Sandbox"
  show TabGame = "Game"
  show TabMods = "Mods"
  show TabSoundpacks = "Soundpacks"
  show TabFonts = "Fonts"
  show TabBackups = "Backups"
  show TabSettings = "Settings"

instance Show CataclysmVariant where
  show CDDA = "Cataclysm: Dark Days Ahead"
  show CBN = "Cataclysm: Bright Night"

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
