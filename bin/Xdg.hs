{-
    Copyright 2017 Markus Ongyerth, Stephan Guenther

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-|
Module      : Xdg
Description : Xdg handler for monky helper application
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux
-}
module Xdg
    ( Xdgdirs (..)
    , getXdgDirs
    )
where

import System.Environment.XDG.BaseDir (getAllDataDirs, getAllConfigDirs, getUserDataDir, getUserCacheDir, getUserConfigDir)
import System.Directory (findFile, doesFileExist, doesDirectoryExist)
import Control.Applicative ((<$>))

data Xdgdirs = Xdgdirs
    { configFile :: FilePath
    , exeFile    :: FilePath
    , cacheDir   :: FilePath
    } deriving (Show)

-- The "" in here are because the api doesn't expect us to cheat :)

{- | Append the directory name keeping search preference.

The monky helper allows files to be either in the monky subdirectory
or directly in the directory, to not change preference we introduce the
alternative form directly behind the primary.

@
[$HOME, /etc/] -> [$HOME/monky/, $HOME/, /etc/monky/, /etc/]
@
-}
appendDirName :: [FilePath] -> String -> [FilePath]
appendDirName [] _ = []
appendDirName (x:xs) y = (x ++ y):x:appendDirName xs y

{- | Find the defining monky.hs file for us.

The helper allows for the config file to be in either $DIR/monky.hs or
$DIR/monky/monky.hs. Both the user config dir and the system config dir
are checked!
-}
findConfigFile :: IO (Either FilePath FilePath)
findConfigFile = do
    dirs <- getAllConfigDirs ""
    found <- findFile (appendDirName dirs "/monky/") "monky.hs"
    case found of
        Just x -> return $ Right x
        Nothing -> Left . (++ "/monky.hs") <$> getUserConfigDir ""

findExecutable :: IO FilePath
findExecutable = do
    dirs <- getAllDataDirs ""
    found <- findFile (appendDirName dirs "/monky/") "/monky.exe"
    case found of
        Just x -> return x
        Nothing -> do
            udir <- getUserDataDir ""
            e <- doesDirectoryExist (udir ++ "/monky/")
            return $ if e then udir ++ "/monky/monky.exe" else udir ++ "/monky.exe"

-- | Get the xdg aware paths. Force if f is given
getXdgDirs :: Bool -> IO (Maybe Xdgdirs)
getXdgDirs f = do
    cache <- getUserCacheDir "monky"
    config <- findConfigFile
    exe <- findExecutable

    e <- doesFileExist exe
    c <- either (const $ return False) (doesFileExist) config
    return $ if e || c || f
       then Just $ Xdgdirs (either id id config) exe cache
       else Nothing
