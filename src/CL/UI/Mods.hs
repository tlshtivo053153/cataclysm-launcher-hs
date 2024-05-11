{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Mods
  ( tabMods
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Mods
import CL.MakeLenses

import Monomer

import qualified Data.Text as T

tabMods :: WidgetNode AppModel AppEvent
tabMods = vstack
  [ hstack
    [ vstack
      [ hstack
        [ filler
        , label "Installed (N hidden):"
        , filler
        ]
      , selectList_ (mods.installed) ["mod_A", "mod_B", "mod_C"] label [onChange $ AppMods . SelectInstalled]
        `styleBasic` [height 200]
      , hstack
        [ filler
        , labeledCheckbox "Show Stock" (mods.isShowStock)
        , filler
        ]
      , hstack
        [ filler
        , button "Delete Selected" $ AppMods DeleteSelected
        , filler
        ]
      ]
    , separatorLine
      `styleBasic` [paddingL 10, paddingR 10]
    , vstack
      [ hstack
        [ filler
        , label "Local repository (N hidden):"
        , filler
        ]
      , selectList_ (mods.installed) ["mod_D", "mod_E", "mod_F"] label [onChange $ AppMods . SelectRepository]
        `styleBasic` [height 200]
      , hstack
        [ filler
        , labeledCheckbox "Show already installed" (mods.isShowInstalled)
        , filler
        ]
      , hstack
        [ filler
        , button "Add Selected" $ AppMods AddSelected
        , spacer
        , button "Add All" $ AppMods AddAll
        , filler
        ]
      , hstack
        [ filler
        , button "Get Kenan Modpack" $ AppMods GetModpack
        , filler
        ]
      ]
    ]
  , vstack $ map label
    [ "Name: mod_name"
    , "Author: mod_author"
    , "Category: mod_category"
    , "Description: mod_descriptoin"
    ]
  ]
