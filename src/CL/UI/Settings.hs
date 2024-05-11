{-# LANGUAGE OverloadedStrings #-}
module CL.UI.Settings
  ( tabSettings
  ) where

import CL.Define (AppModel(..), AppEvent(..))
import CL.Define.Settings
import CL.MakeLenses

import Monomer

import qualified Data.Text as T

tabSettings :: WidgetNode AppModel AppEvent
tabSettings = label "Not yet implemented"
