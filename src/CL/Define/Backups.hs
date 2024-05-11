module CL.Define.Backups
  ( BackupsModel(..)
  , BackupsEvent(..)
  ) where

import qualified Data.Text as T

data BackupsModel = BackupsModel
  { _backupsModelSaveDirectory :: T.Text
  , _backupsModelManualBackupName :: T.Text
  }
  deriving (Eq, Show)

data BackupsEvent
  = SelectBackup T.Text
  | Restore
  | Delete
  | Refresh
  | Create
  deriving (Eq, Show)
