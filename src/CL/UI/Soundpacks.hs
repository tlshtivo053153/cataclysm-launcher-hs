{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Soundpacks
  ( tabSoundpacks
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Soundpacks
import CL.MakeLenses

import Monomer

import qualified Data.Text as T

tabSoundpacks :: WidgetNode AppModel AppEvent
tabSoundpacks = vstack
  [ hstack
    [ vstack
      [ hstack
        [ filler
        , label "Installed:"
        , filler
        ]
      , selectList_ (soundpacks.installed) ["sound_A", "sound_B" ] label [onChange $ AppSoundpacks . SelectInstalled ]
        `styleBasic` [height 200]
      , hstack
        [ filler
        , labeledCheckbox "Show Stock" (soundpacks.isShowStock)
        , filler
        ]
      , hstack
        [ filler
        , button "Delete Selected" $ AppSoundpacks DeleteSelected
        , filler
        ]
      ]
    , separatorLine
      `styleBasic` [paddingL 10, paddingR 10]
    , vstack
      [ hstack
        [ filler
        , label "Downloadable:"
        , filler
        ]
      , selectList_ (soundpacks.downloadable) ["sound_C", "sound_D"] label [onChange $ AppSoundpacks . SelectDownloadable]
        `styleBasic` [height 200]
      , hstack
        [ filler
        , button "Install Selected" $ AppSoundpacks InstallSelected
        , filler
        ]
      ]
    ]
  ]
