{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Sandbox
  ( tabSandbox
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Sandbox
import CL.MakeLenses

import Control.Lens
import Monomer

tabSandbox :: AppModel -> WidgetNode AppModel AppEvent
tabSandbox model = vstack
  [ label "Active Sandbox:"
  , label $ model ^. sandbox.activeSandbox
  , hstack
    [ vstack
      [ selectList_ (sandbox.sandboxName) (model^.sandbox.sandboxList) label []
      , button "switch" $ AppSandbox Switch
      , button "new" $ AppSandbox New
      , button "copy" $ AppSandbox Copy
      , button "rename" $ AppSandbox Rename
      ]
      `styleBasic` [padding 5]
    , separatorLine
      `styleBasic` [padding 5]
    , vstack
      [ label "Infomation:"
      , label_ "infomation" [ellipsis]
        `styleBasic`
          [ border 4 lightGray
          , radius 10
          , flexHeight 50
          ]
      ]
      `styleBasic` [padding 5]
    ]
  ]
