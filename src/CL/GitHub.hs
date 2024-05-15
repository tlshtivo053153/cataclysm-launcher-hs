{-# LANGUAGE OverloadedStrings #-}
module CL.GitHub
  ( getGitHubReleases
  ) where

import GitHub.REST
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson

state :: GitHubSettings
state = GitHubSettings
  { token = Nothing
  , userAgent = "tlshtivo053153/cataclysm-launcher-hs"
  , apiVersion = "2022-11-28"
  }

-- https://docs.github.com/en/rest/releases/releases
getGitHubReleases :: Text -> Text -> IO Value
getGitHubReleases owner repo = do
  runGitHubT state $
    queryGitHub GHEndpoint
      { method = GET
      , endpoint = "/repos/:owner/:repo/releases"
      , endpointVals =
          [ "owner" := owner
          , "repo" := repo
          ]
      , ghData = []
      }
