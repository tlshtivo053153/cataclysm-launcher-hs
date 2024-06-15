module CL.Define.Sandbox
  ( SandboxModel(..)
  , SandboxEvent(..)
  ) where

import qualified Data.Text as T

import Data.Default
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics

import System.Info

import CL.Define.Core

data SandboxModel = SandboxModel
  { _sandboxModelSandboxName :: T.Text
  , _sandboxModelSandboxList :: [T.Text]
  , _sandboxModelActiveSandbox :: T.Text
  , _sandboxModelIsMakingSandbox :: Bool
  , _sandboxModelNewName :: T.Text
  , _sandboxModelNewVariant :: CataclysmVariant
  , _sandboxModelNewVersion :: CataclysmVersion
  }
  deriving (Eq, Show)

data SandboxEvent = Switch
                  | New
                  | Copy
                  | Rename
                  | AddSandbox T.Text
                  | MakeOK
                  | MakeCancel
  deriving (Eq, Show)
