{-# LANGUAGE OverloadedStrings #-}
module CL.Event.Sandbox
  ( eventSandbox
  ) where

import CL.Define
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.Define.Sandbox
import CL.MakeLenses
import CL.UI.Game
import CL.UI.Mods
import CL.UI.Soundpacks
import CL.UI.Fonts
import CL.UI.Backups
import CL.UI.Settings
import CL.UI.Sandbox
import qualified CL.Define.Game as G

import Control.Lens
import Monomer

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Req
import qualified GitHub
import qualified Text.URI as URI

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List as L

import System.Directory
import Codec.Archive.Zip
import qualified Codec.Compression.GZip as GZip
--import qualified Codec.Archive.Tar as Tar
import qualified Data.Conduit.Tar as Tar
import Data.Conduit
import Data.Conduit.Zlib
import Conduit

import System.Process
import Control.Exception.Safe

eventSandbox :: AppModel -> SandboxEvent -> [AppEventResponse AppModel AppEvent]
eventSandbox model Switch =
  [ Model $ model & sandbox.activeSandbox .~ (model ^. sandbox.sandboxName) ]
eventSandbox model _ = []
