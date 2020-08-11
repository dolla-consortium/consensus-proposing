{-# LANGUAGE RecordWildCards #-}
module Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
  ( ExecutableSettingsProvider (..)
  , ExecutableSettings (..)
  , FileSystemLocation (..)
  , RootFolder
  , FileName
  , ShellCommand
  , saveConfigurationsAndCreateLogFolder
  , configurationFilePath
  , getRunCommand
  , getLogFilePath
  ) where

import           Prelude hiding (writeFile)
import           Data.ByteString.Lazy (ByteString,writeFile)
import           System.Directory
import           Dolla.Common.Data.IsString ((<++>))
import           Control.Monad.Reader


type RootFolder = FilePath
type FileName = FilePath
type ShellCommand = String

class  ExecutableSettingsProvider microserviceSetting   where
  getExecutableSettings :: microserviceSetting -> ExecutableSettings

data ExecutableSettings
  = ExecutableSettings
    { executableName ::  String
    , executableSettings :: ByteString
    , logFileLocation :: FileSystemLocation
    , configurationLocation :: FileSystemLocation}

data FileSystemLocation
  = FileSystemLocation
    { rootFolder :: RootFolder
    , fileName :: FileName}

configurationFilePath :: FileSystemLocation -> FilePath
configurationFilePath FileSystemLocation {..} = rootFolder ++ fileName


getRunCommand :: ExecutableSettings  -> ShellCommand
getRunCommand settings
  = executableName settings <++> " " <++> configurationFilePath (configurationLocation settings)

createLogFolder 
  :: MonadIO m 
  => ExecutableSettings
  -> m ()
createLogFolder ExecutableSettings {logFileLocation = FileSystemLocation {..}, ..} =
 liftIO $ createDirectoryIfMissing True rootFolder 
 
 
getLogFilePath :: ExecutableSettings  -> ShellCommand
getLogFilePath settings = configurationFilePath (logFileLocation settings) 

saveConfigurationsAndCreateLogFolder 
  :: MonadIO m
  => ExecutableSettings
  -> m ExecutableSettings
saveConfigurationsAndCreateLogFolder settings@ ExecutableSettings {configurationLocation = configurationLocation@FileSystemLocation {..}, ..} = do
  let filePath = configurationFilePath configurationLocation
  _ <- createLogFolder settings 
  liftIO $ createDirectoryIfMissing True rootFolder
  liftIO $ writeFile filePath executableSettings
  return settings