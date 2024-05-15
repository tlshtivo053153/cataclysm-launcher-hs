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

import qualified CL.GitHub as GH

import Control.Lens
import Monomer

import Data.Aeson.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Network.HTTP.Req
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
          , Task $ AppGame . RefreshAvailable <$> (getAvailables `catch` (\e -> print (e :: IOException) >> return []))
          ]
eventGame model (RefreshAvailable []) = [ Model $ model & isLoading .~ False ]
eventGame model (RefreshAvailable cs) =
  [ Model $ model & game.availables .~ cs
                  & isLoading .~ False
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
  [ Model $ model & isLoading .~ False
                  & game.installedVersion %~ (model^.game.selectAvailable :)
  ]
eventGame model Play =
  [ Model $ model & isLoading .~ True
  , Task $ do
      playGame model
        `catchIO` (\e -> print e >> putStrLn "error: playGame")
      return $ AppGame EndPlay
  ]
eventGame model EndPlay =
  [ Model $ model & isLoading .~ False
  ]
eventGame model Active =
  [ Model $ model & game.activeInstall .~ (model^.game.version) ]
eventGame _ _ = []

getAvailables :: IO [Available]
getAvailables = do
  releases <- GH.getGitHubReleases "CleverRaven" "Cataclysm-DDA"
  let avail = toAvailables releases
  case avail of
    Just avail' -> do
      writeFileAvailables avail'
      return avail'
    Nothing -> return []

toAvailables :: Value -> Maybe [Available]
toAvailables v = do
  v' <- v ^? _Array
  return $ V.toList $ V.catMaybes $ fmap toAvail v'
    where
      toAvail x = do
        name' <- x ^? key "name" . _String
        tag' <- x ^? key "tag_name" . _String
        assets <- x ^? key "assets" . _Array
        win <- findUrl assets "cdda-windows-tiles-x64-"
        winMsvc <- findUrl assets "cdda-windows-tiles-x64-msvc-"
        linux <- findUrl assets "cdda-linux-tiles-x64-"
        Just $ Available
          { _availableName = name'
          , _availableTag = tag'
          , _availableUrlWindows = win
          , _availableUrlWindowsMsvc = winMsvc
          , _availableUrlLinux = linux
          }
      findUrl assets platform' = do
        let p asset =
              case asset ^? key "name" . _String of
                Just name' -> platform' `T.isPrefixOf` name'
                Nothing -> False
        a <- V.find p assets
        a ^? key "browser_download_url" . _String

writeFileAvailables :: [Available] -> IO ()
writeFileAvailables [] = putStrLn "available is empty"
writeFileAvailables as =
  BL.writeFile "launcher_available.json" $ encodePretty as

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

playGame :: AppModel -> IO ()
playGame model = do
  let path = "sys-repo"</>"game"</> T.unpack (model^.game.activeInstall) </> "cataclysmdda-0.I"
  userPath <- do
    cd <- getCurrentDirectory
    let sandboxName' = T.unpack $ model^.sandbox.activeSandbox
    return $ cd </> "sandbox" </> sandboxName' <> [pathSeparator]
  doesExistProgram <- doesFileExist $ path </> "cataclysm-tiles"
  when doesExistProgram $ callProcess (path </> "cataclysm-launcher") ["--userdir", userPath]
