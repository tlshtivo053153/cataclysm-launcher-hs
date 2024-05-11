module CL.Define.Fonts
  ( FontsModel(..)
  , FontsEvent(..)
  ) where

import qualified Data.Text as T

data FontsModel = FontsModel
  { _fontsModelAvailable :: T.Text
  , _fontsModelIsPreviewCyrillic :: Bool
  , _fontsModelFontSizeUI :: Int
  , _fontsModelFontSizeMap :: Int
  , _fontsModelFontSizeOvermap :: Int
  , _fontsModelFontBlending :: Bool
  }
  deriving (Eq, Show)

data FontsEvent
  = SelectAvailable T.Text
  | SetFontGame
  | SetFontMap
  | SetFontOvermap
  | SetFontAll
  deriving (Eq, Show)
