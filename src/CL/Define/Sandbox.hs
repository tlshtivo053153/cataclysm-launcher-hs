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

data SandboxModel = SandboxModel
  { _sandboxModelSandboxName :: T.Text
  , _sandboxModelSandboxList :: [T.Text]
  , _sandboxModelActiveSandbox :: T.Text
  }
  deriving (Eq, Show)

data SandboxEvent = Switch
                  | New
                  | Copy
                  | Rename
  deriving (Eq, Show)
