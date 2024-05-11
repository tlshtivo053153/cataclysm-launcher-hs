{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module CL.Event.Game
  ( eventGame
  ) where

import CL.Define
import CL.Define.Game
import CL.Define.Mods
import CL.Define.Soundpacks
import CL.Define.Fonts
import CL.Define.Backups
import CL.Define.Settings
import CL.MakeLenses
import CL.UI.Game
import CL.UI.Mods
import CL.UI.Soundpacks
import CL.UI.Fonts
import CL.UI.Backups
import CL.UI.Settings
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

eventGame :: AppModel -> GameEvent -> [AppEventResponse AppModel AppEvent]
eventGame model G.Refresh =
  if model ^. game.channel == Stable
     then []
     else [ Model $ model & isLoading .~ True
          , Task $ AppGame . RefreshAvailable <$> (getAvailable `catch` (\e -> print (e :: IOException) >> return []))
          ]
eventGame model (RefreshAvailable []) = [ Model $ model & isLoading .~ False ]
eventGame model (RefreshAvailable cs) =
  [ Model $ model & game.availables .~ cs
  , Model $ model & isLoading .~ False
  ]
eventGame model Install =
  [ Model $ model & isLoading .~ True
  , Task $ do
      putStrLn "Start Event Game Install"
      let avail = L.find (\x -> x^.name == model^.game.selectAvailable) $ model^.game.availables
      putStrLn $ "avail: " ++ show avail
      case avail of
        Just avail' -> installAvailable (model^.game.platform) avail'
                            `catchIO` (\e -> print e >> putStrLn "error: installAvailable")
        Nothing -> return ()
      putStrLn "End Event Game Install"
      return $ AppGame EndInstall
  ]
eventGame model EndInstall =
  [ Model $ model & isLoading .~ False ]
eventGame model Play =
  [ Model $ model & isLoading .~ True
  , Task $ do
      playGame (model^.game)
        `catchIO` (\e -> print e >> putStrLn "error: playGame")
      return $ AppGame EndPlay
  ]
eventGame model EndPlay =
  [ Model $ model & isLoading .~ False
  ]
eventGame model Active =
  [ Model $ model & game.activeInstall .~ (model^.game.version) ]
eventGame _ _ = []

getAvailable :: IO [Available]
getAvailable = do
  releases <- GitHub.github' (GitHub.releasesR "CleverRaven" "Cataclysm-DDA") 1
  let avails =
        case releases of
          Left _ -> []
          Right r -> V.toList $ V.mapMaybe toAvailable r
  writeFileAvailable avails
  return avails

--getCommits :: IO [GithubCommit]
--getCommits = do
--  c <- GitHub.github' (GitHub.commitsForR "CleverRaven" "Cataclysm-DDA") 10
--  return $ case c of
--    Left _ -> []
--    Right c' -> []

toAvailable :: GitHub.Release -> Maybe Available
toAvailable r = do
  win <- findUrl "cdda-windows-tiles-x64-"
  winMsvc <- findUrl "cdda-windows-tiles-x64-msvc-"
  linux <- findUrl "cdda-linux-tiles-x64-"
  return Available
    { _availableName = GitHub.releaseName r
    , _availableTag = GitHub.releaseTagName r
    , _availableUrlWindows = win
    , _availableUrlWindowsMsvc = winMsvc
    , _availableUrlLinux = linux
    }
    where
      assets = GitHub.releaseAssets r
      findUrl env =
        GitHub.releaseAssetBrowserDownloadUrl
          <$> V.find (\x -> env `T.isPrefixOf` GitHub.releaseAssetName x) assets

writeFileAvailable :: [Available] -> IO ()
writeFileAvailable [] = return ()
writeFileAvailable as =
  BL.writeFile "launcher_available.json" $ encodePretty as

--toGithubCommit :: GitHub.Commit -> GithubCommit
--toGithubCommit c = GithubCommit
--  { _githubCommitDate = undefined
--  , _githubCommitHash = T.pack $ show $ GitHub.commitSha c
--  , _githubCommitUrlWindows = undefined
--  , _githubCommitUrlLinux = undefined
--  }
--    where
--      gitCommit = GitHub.commitGitCommit c

installAvailable :: Platform -> Available -> IO ()
installAvailable p avail = do
  (uri, fname) <- do
    let urlSys = case p of
              Windows -> avail ^. urlWindows
              WindowsMsvc -> avail ^. urlWindowsMsvc
              Linux -> avail ^. urlLinux
              _ -> avail ^. urlLinux
    uriSys <- URI.mkURI urlSys
    return (uriSys, takeFileName $ T.unpack urlSys)
  case useHttpsURI uri of
    Just (url, options) -> do
      putStrLn $ "url: " ++ show url
      res <- runReq defaultHttpConfig $ do
        response <- req GET url NoReqBody lbsResponse $ options <> port 443
        liftIO $ BL.writeFile ("cache"</>"game"</> fname ) $ responseBody response
        return $ responseBody response
      if ".zip" `L.isSuffixOf` fname
        then extractWin (T.unpack $ avail^.tag) res
--        else extractLinux (T.unpack $ avail^.name) res
        else extractLinux (T.unpack $ avail^.tag) ("cache"</>"game"</> fname )
    Nothing -> return ()

-- .zip
extractWin repoName bs = do
  let path = "sys-repo"</>"game"</> repoName
      opt = [OptDestination path]
  createDirectoryIfMissing True path
  extractFilesFromArchive opt $ toArchive bs

-- .tar.gzip
extractLinux repoName archive = do
  let path = "sys-repo"</>"game"</> repoName
--      bs' = GZip.decompress bs
  createDirectoryIfMissing True path
  _ <- runConduitRes $
    sourceFileBS archive
    .| ungzip
    .| Tar.untarWithExceptions (Tar.restoreFileIntoLenient path)
  return ()
--  Tar.unpack path $ Tar.read bs'
--  Tar.extractTarball archive (Just path)
--  r <- Tar.extractTarballLenient archive (Just path)

playGame :: GameModel -> IO ()
playGame model = do
  let path = "sys-repo"</>"game"</> T.unpack (model^.activeInstall) </> "cataclysmdda-0.I"
  userPath <- do
    cd <- getCurrentDirectory
    return $ cd </> "sandbox" </> "sandbox1" <> [pathSeparator]
  doesExistProgram <- doesFileExist $ path </> "cataclysm-tiles"
  when doesExistProgram $ callProcess (path </> "cataclysm-launcher") ["--userdir", userPath]
