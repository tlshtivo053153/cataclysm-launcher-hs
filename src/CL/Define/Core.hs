{-# LANGUAGE OverloadedStrings #-}
module CL.Define.Core
  ( CataclysmVariant(..)
  , SelectTab(..)
  , CataclysmVersion(..)
  , DDAVersion(..)
  , BNVersion(..)
  ) where

data CataclysmVariant
  = CDDA
  | CBN
  deriving (Eq)

instance Show CataclysmVariant where
  show CDDA = "Cataclysm: Dark Days Ahead"
  show CBN = "Cataclysm: Bright Night"

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

data CataclysmVersion
  = VDDA DDAVersion
  | VBN BNVersion
  deriving (Eq, Show)

data DDAVersion
  = DDAVerExperimental
  | DDAVer0D
  | DDAVer0E
  | DDAVer0F
  | DDAVer0G
  deriving (Eq, Show)

data BNVersion
  = BNExperimental
  deriving (Eq, Show)
