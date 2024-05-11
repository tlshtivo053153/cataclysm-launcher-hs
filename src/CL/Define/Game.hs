{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module CL.Define.Game
  ( Channel(..)
  , Available(..)
  , Platform(..)
  , GameModel(..)
  , GameEvent(..)
  ) where

import qualified Data.Text as T

import Data.Default
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics

import System.Info

data Channel = Stable | Experimental
  deriving (Eq, Show)

--data GithubCommit = GithubCommit
--  { _githubCommitDate :: T.Text
--  , _githubCommitHash :: T.Text
--  , _githubCommitUrlWindows :: T.Text
--  , _githubCommitUrlLinux :: T.Text
--  }
--  deriving (Eq, Show)

data Available = Available
  { _availableName :: T.Text
  , _availableTag :: T.Text
  , _availableUrlWindows :: T.Text
  , _availableUrlWindowsMsvc :: T.Text
  , _availableUrlLinux :: T.Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Available where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Available where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
  
data Platform = Windows | WindowsMsvc | Linux | UnknownPlatform
  deriving (Eq, Show)

data GameModel = GameModel
  { _gameModelChannel :: Channel
  , _gameModelSelectAvailable :: T.Text
  , _gameModelVersion :: T.Text
  , _gameModelActiveInstall :: T.Text
  , _gameModelAvailables :: [Available]
  , _gameModelInstalledVersion :: [T.Text]
  , _gameModelPlatform :: Platform
  }
  deriving (Eq, Show)

instance Default GameModel where
  def = GameModel
    { _gameModelChannel = Experimental
    , _gameModelSelectAvailable = ""
    , _gameModelVersion = ""
    , _gameModelActiveInstall = ""
    , _gameModelAvailables = []
    , _gameModelInstalledVersion = []
    , _gameModelPlatform = case os of
                             "linux" -> Linux
                             "mingw32" -> WindowsMsvc
                             _ -> UnknownPlatform
    }

data GameEvent
  = Refresh
  | RefreshAvailable [Available]
  | Install
  | EndInstall
  | Play
  | EndPlay
  | Active
  | Delete
  | SelectVersion T.Text
  deriving (Eq, Show)
