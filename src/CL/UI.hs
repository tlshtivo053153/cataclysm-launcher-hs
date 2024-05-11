{-# LANGUAGE OverloadedStrings #-}
module CL.UI
  ( buildUI
  ) where

import CL.Define
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.Define.Sandbox
import CL.MakeLenses
import CL.UI.Game
import CL.UI.Mods
import CL.UI.Soundpacks
import CL.UI.Fonts
import CL.UI.Backups
import CL.UI.Settings
import CL.UI.Sandbox
import CL.Event

import Control.Lens
import Monomer
import qualified Monomer.Lens as L

import qualified Data.Text as T


makeTab :: AppModel -> SelectTab -> WidgetNode s AppEvent
makeTab model tab =
  if model ^. selectedTab == tab
    then button'
    else button' `styleBasic` [ bgColor dimGray ]
  where
    button' = button (T.pack $ show tab) (AppSelectTab tab)

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree `styleBasic` [padding 10] where
  widgetTree = zstack
    [ widgetMain
    , widgetLoading
    ]
    --, box (label "Now Loading...")
  widgetMain = vstack
    [ hstack
      [ label "Game:"
      , spacer
      , textDropdownS cataclysmVariant [CDDA, CBN]
      ]
    , hstack $ map (makeTab model)
      [ TabSandbox
      , TabGame
      , TabMods
      , TabSoundpacks
      , TabFonts
      , TabBackups
      , TabSettings
      ]
    , zstack
      [ tabSandbox model `nodeVisible` (model ^. selectedTab == TabSandbox)
      , tabGame model `nodeVisible` (model ^. selectedTab == TabGame)
      , tabMods `nodeVisible` (model ^. selectedTab == TabMods)
      , tabSoundpacks `nodeVisible` (model ^. selectedTab == TabSoundpacks)
      , tabFonts `nodeVisible` (model ^. selectedTab == TabFonts)
      , tabBackups `nodeVisible` (model ^. selectedTab == TabBackups)
      , tabSettings `nodeVisible` (model ^. selectedTab == TabSettings)
      ]
      `styleBasic` [padding 5]
    ]
  widgetLoading =
    let loadingLabel = label "Now Loading..." `styleBasic` [textSize 20, textColor black]
     in box loadingLabel `styleBasic` [bgColor (darkGray & L.a .~ 0.8)]
          `nodeVisible` (model ^. isLoading)
