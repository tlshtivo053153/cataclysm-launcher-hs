{-# LANGUAGE OverloadedStrings #-}
module CL.Event
  ( handleEvent
  ) where

import CL.Define
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.MakeLenses
import CL.UI.Game
import CL.UI.Mods
import CL.UI.Soundpacks
import CL.UI.Fonts
import CL.UI.Backups
import CL.UI.Settings
import CL.Event.Game
import CL.Event.Mods
import CL.Event.Soundpacks
import CL.Event.Fonts
import CL.Event.Backups
import CL.Event.Settings
import CL.Event.Sandbox

import Control.Lens
import Monomer

import qualified Data.Text as T

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppSelectTab t ->
    [ Model (model & selectedTab .~ t) ]
  AppGame e -> eventGame model e
--  AppGame Refresh -> []
--  AppGame Install -> []
--  AppGame Play -> []
--  AppGame Active -> []
--  AppGame Delete -> []
--  AppGame (SelectVersion _)-> []
  AppMods _ -> []
  AppSoundpacks _ -> []
  AppFonts _ -> []
  AppBackups _ -> []
  AppSettings _ -> []
  AppSandbox e -> eventSandbox model e
