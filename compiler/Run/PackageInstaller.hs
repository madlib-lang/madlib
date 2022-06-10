module Run.PackageInstaller where

import           GHC.IO                         ( )
import           System.FilePath                ( takeDirectory )
import           System.Process
import           Control.Exception              ( try )
import           System.Environment             ( getEnv )
import           System.Environment.Executable  ( getExecutablePath )
import           Run.CommandLine


runPackageInstaller :: IO ()
runPackageInstaller = do
  executablePath              <- getExecutablePath
  packageInstallerPath        <- try $ getEnv "PKG_INSTALLER_PATH"
  packageInstallerPathChecked <- case (packageInstallerPath :: Either IOError String) of
    Left _ -> do
      return $ takeDirectory executablePath <> "/package-installer.js"
    Right p -> return p

  callCommand $ "node " <> packageInstallerPathChecked
