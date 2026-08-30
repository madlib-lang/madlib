module Run.PackageInstaller where

import           GHC.IO                         ( )
import           System.FilePath                ( takeDirectory )
import           System.Process                 ( callProcess )
import           Control.Exception              ( try )
import           System.Environment             ( getEnv )
import           System.Environment.Executable  ( getExecutablePath )


runPackageInstaller :: Bool -> Bool -> Maybe String -> Bool -> [String] -> IO ()
runPackageInstaller graph upgrade mWhy interactive resolutions = do
  executablePath              <- getExecutablePath
  packageInstallerPath        <- try $ getEnv "PKG_INSTALLER_PATH"
  packageInstallerPathChecked <- case (packageInstallerPath :: Either IOError String) of
    Left _ -> do
      return $ takeDirectory executablePath <> "/package-installer.js"
    Right p -> return p

  let extraArgs = concat [ if graph   then ["--graph"]   else []
                         , if upgrade then ["--upgrade"] else []
                         , maybe [] (\pkg -> ["--why", pkg]) mWhy
                         , if interactive then ["--interactive"] else []
                         , concatMap (\resolution -> ["--resolve", resolution]) resolutions
                         ]
  callProcess "node" (packageInstallerPathChecked : extraArgs)
