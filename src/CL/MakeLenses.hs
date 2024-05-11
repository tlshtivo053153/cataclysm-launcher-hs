{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module CL.MakeLenses
  ( module CL.MakeLenses
  ) where

import Control.Lens

import CL.Define
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.Define.Sandbox

makeFields ''AppModel
makeFields ''Available
makeFields ''GameModel
makeFields ''ModsModel
makeFields ''SoundpacksModel
makeFields ''FontsModel
makeFields ''BackupsModel
makeFields ''SettingsModel
makeFields ''SandboxModel
