{-# LANGUAGE OverloadedStrings #-}
module CL.Main
  ( mainLoop
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
import CL.Event
import CL.UI

import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Control.Lens
import Data.Aeson
import Monomer
import Control.Exception.Safe

import System.FilePath ( (</>) )
import System.Directory
import qualified Data.Text as T

mainLoop :: IO ()
mainLoop = do
  initialize
  config <- makeConfig
  model <- makeModel
  startApp model handleEvent buildUI config

initialize :: IO ()
initialize = do
  let repo = [ "game", "mods", "soundpacks", "tilesets" ]
   in mapM_ (createDirectoryIfMissing True) $ concat
        [ [ "sys-repo", "user-repo", "cache", "sandbox" ]
        , [ "sys-repo" </> r | r <- repo ]
        , [ "user-repo" </> r | r <- repo ]
        , [ "cache" </> r | r <- repo ]
        ]

makeConfig :: IO [AppConfig AppModel AppEvent]
makeConfig = return
    [ appWindowTitle "Cataclysm Launcher HS"
    , appTheme darkTheme
    , appFontDef "Regular" "./assets/fonts/HackGen-Regular.ttf"
--      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
--      , appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf"
--      , appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
--      , appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf"
    , appInitEvent AppInit
    , appDisableAutoScale True
--      , appScaleFactor 2
    ]

makeModel :: IO AppModel
makeModel = do
  doesFileAvailable <- doesFileExist "launcher_available.json"
  let readFileAvailable path = BL.readFile path
        `catchIO` const (return BL.empty)
  gameAvailable <- decode <$> if doesFileAvailable
    then readFileAvailable "launcher_available.json"
    else return BL.empty
  listGame <- listDirectory "sys-repo/game"
  listSandbox <- do
    dir <- listDirectory "sandbox"
    if null dir
       then do
         let name' = "sandbox1"
         createDirectoryIfMissing True $ "sandbox" </> name'
         return [name']
       else return dir
  return $ AppModel
    { _appModelCataclysmVariant = CDDA
    , _appModelSelectedTab = TabGame
    , _appModelIsLoading = False
    , _appModelGame = def
        & availables .~ fromMaybe [] gameAvailable
        & installedVersion .~ map T.pack listGame
    , _appModelMods = ModsModel
      { _modsModelInstalled = ""
      , _modsModelIsShowStock = False
      , _modsModelIsShowInstalled = False
      }
    , _appModelSoundpacks = SoundpacksModel
      { _soundpacksModelInstalled = ""
      , _soundpacksModelIsShowStock = False
      , _soundpacksModelDownloadable = ""
      }
    , _appModelFonts = FontsModel
      { _fontsModelAvailable = ""
      , _fontsModelIsPreviewCyrillic = False
      , _fontsModelFontSizeUI = 16
      , _fontsModelFontSizeMap = 16
      , _fontsModelFontSizeOvermap = 16
      , _fontsModelFontBlending = True
      }
    , _appModelBackups = BackupsModel
      { _backupsModelSaveDirectory = ""
      , _backupsModelManualBackupName = "backup_xxxx-xx-xx"
      }
    , _appModelSettings = SettingsModel
      { _settingsModelAnysetting = 0
      }
    , _appModelSandbox = SandboxModel
      { _sandboxModelSandboxName = ""
      , _sandboxModelSandboxList = map T.pack listSandbox
      , _sandboxModelActiveSandbox = ""
      }
    }
