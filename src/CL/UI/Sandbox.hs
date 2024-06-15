{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Sandbox
  ( tabSandbox
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Core
import CL.Define.Sandbox
import CL.MakeLenses

import qualified Data.Text as T
import Control.Lens
import Monomer
import qualified Monomer.Lens as Lens

tabSandbox :: AppModel -> WidgetNode AppModel AppEvent
tabSandbox model = zstack
  [ vstack
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
    `nodeVisible` not (model^.sandbox.isMakingSandbox)
  , sandboxNew model
    `nodeVisible` (model^.sandbox.isMakingSandbox)
  ]

sandboxNew :: AppModel -> WidgetNode AppModel AppEvent
sandboxNew model = vstack
  [ hstack
    [ label "Name:"
    , spacer
    , textField (sandbox.newName)
    ]
  , hstack
    [ label "Variant:"
    , spacer
    , textDropdownS (sandbox.newVariant) [CDDA, CBN]
    ]
  , hstack
    [ label "Version:"
    , spacer
    , let vs = map VDDA [DDAVerExperimental, DDAVer0C, DDAVer0D, DDAVer0E, DDAVer0F, DDAVer0G]
      in textDropdownS (sandbox.newVersion) vs
    ]
  , hstack
    [ filler
    , button "OK" $ AppSandbox MakeOK
    , spacer
    , button "Cancel" $ AppSandbox MakeCancel
    ]
  ]
