{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Backups
  ( tabBackups
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Backups
import CL.MakeLenses

import Monomer

import qualified Data.Text as T

tabBackups :: WidgetNode AppModel AppEvent
tabBackups = vstack
  [ label "Save directory backups:"
  , hstack
    [ selectList_ (backups.saveDirectory) ["save_A", "save_B"] label [onChange $ AppBackups . SelectBackup]
    , label "infomation backup directory"
      `styleBasic`
        [ border 4 lightGray
        , flexHeight 30
        ]
    ]
  , hstack
    [ button "Restore" $ AppBackups Restore
    , button "Delete" $ AppBackups Delete
    , button "Refresh" $ AppBackups Refresh
    ]
  , separatorLine
  , label "Manual backup"
  , hstack
    [ label "Name:"
    , textField $ backups.manualBackupName
    , button "Create" $ AppBackups Create
    ]
  ]
