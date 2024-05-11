module CL.Define.Mods
  ( ModsModel(..)
  , ModsEvent(..)
  ) where

import qualified Data.Text as T

data ModsModel = ModsModel
  { _modsModelInstalled :: T.Text
  , _modsModelIsShowStock :: Bool
  , _modsModelIsShowInstalled :: Bool
  }
  deriving (Eq, Show)

data ModsEvent
  = SelectInstalled T.Text
  | DeleteSelected
  | SelectRepository T.Text
  | AddSelected
  | AddAll
  | GetModpack
  deriving (Eq, Show)
