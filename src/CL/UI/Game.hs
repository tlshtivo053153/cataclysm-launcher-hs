{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Game
  ( tabGame
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Game
import CL.MakeLenses

import Control.Lens
import Monomer

import qualified Data.Text as T

tabGame :: AppModel -> WidgetNode AppModel AppEvent
tabGame model = vstack
  [ label "Channel:"
    `styleBasic` [paddingT 10, paddingB 10]
  , hstack
    [ labeledRadio_ "Stable" Stable (game.channel) [textRight]
      `styleBasic` [ paddingR 10 ]
    , labeledRadio_ "Experimental" Experimental (game.channel) [textRight]
    ]
  , hstack
    [ label "Available builds:"
    , textDropdown (game.selectAvailable) $ (model^.game.availables) & map (^.name)
    , button "Refresh" $ AppGame Refresh
    ]
  , hstack
    [ filler
    , button "Install Selected" $ AppGame Install
    , filler
    ]
    `styleBasic` [ padding 5 ]
  , separatorLine
    `styleBasic` [ padding 10 ]
  , hstack
    [ filler
    , label "Active Install:"
    , filler
    ]
    `styleBasic` [ padding 5 ]
  , hstack
    [ filler
    , label $ model ^. game.activeInstall
    , filler
    ]
  , hstack
    [ filler
    , button "Play" $ AppGame Play
    , filler
    ]
  , separatorLine
    `styleBasic` [ padding 10 ]
  , hstack
    [ filler
    , label "Installed version:"
    , filler
    ]
  , hstack
--    [ selectList_ (game.version) (map (\x -> T.pack $ "version" <> show x) [1..10 :: Int]) label [onChange $ AppGame . SelectVersion]
    [ selectList_ (game.version) (model ^. game.installedVersion) label [onChange $ AppGame . SelectVersion]
    , vstack
      [ button "Make Active" $ AppGame Active
      , button "Delete" $ AppGame Delete
      ]
    ]
  ]
