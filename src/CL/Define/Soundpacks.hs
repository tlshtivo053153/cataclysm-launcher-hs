module CL.Define.Soundpacks
  ( SoundpacksModel(..)
  , SoundpacksEvent(..)
  ) where

import qualified Data.Text as T

data SoundpacksModel = SoundpacksModel
  { _soundpacksModelInstalled :: T.Text
  , _soundpacksModelIsShowStock :: Bool
  , _soundpacksModelDownloadable :: T.Text
  }
  deriving (Eq, Show)

data SoundpacksEvent
  = SelectInstalled T.Text
  | DeleteSelected
  | SelectDownloadable T.Text
  | InstallSelected
  deriving (Eq, Show)
