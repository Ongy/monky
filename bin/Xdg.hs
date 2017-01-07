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

import System.Environment.XDG.BaseDir
import System.Directory (findFile, doesFileExist, doesDirectoryExist)
import Control.Applicative ((<$>))

data Xdgdirs = Xdgdirs
    { configFile :: FilePath
    , cacheDir   :: FilePath
    , exeFile    :: FilePath
    } deriving (Show)

data Conf = Defaulted FilePath | User FilePath | System FilePath

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
findConfigFile :: IO Conf
findConfigFile = do
    dir <- getUserConfigDir ""
    fuser <- findFile (appendDirName [dir] "/monky/") "monky.hs"
    dirs <- getSystemConfigDirs ""
    fsys <- findFile (appendDirName dirs "/monky/") "monky.hs"
    return $ case fuser of
        Just x -> User x
        Nothing -> case fsys of
            Just x -> System x
            Nothing -> Defaulted $ dir ++ "/monky.hs"

findUserExecutable :: IO FilePath
findUserExecutable = do
    dir <- getUserDataDir ""
    e <- doesDirectoryExist (dir ++ "/monky/")
    if e
       then return $ dir ++ "/monky/monky.exe"
       else return $ dir ++ "/monky.exe"

-- | Default to user path, it should be the only one where we have +w
findExecutable :: IO FilePath
findExecutable = do
    path <- findUserExecutable
    e <- doesFileExist path
    if e
       -- Use user path if the file exists!
       then return path
       else do
            -- try system dirs, if none exists, return user path
            dirs <- getSystemDataDirs ""
            found <- findFile (appendDirName dirs "/monky/") "/monky.exe"
            case found of
                Just x -> return x
                Nothing -> return path

-- | Get the xdg aware paths. Force if f is given
getXdgDirs :: Bool -> IO (Maybe Xdgdirs)
getXdgDirs f = do
    cache <- getUserCacheDir "monky"
    config <- findConfigFile

    case config of
        User conf -> Just . Xdgdirs conf cache <$> findUserExecutable
        System conf -> Just . Xdgdirs conf cache <$> findExecutable
        Defaulted conf -> do
            exe <- findExecutable
            e <- doesFileExist exe
            return $ if f || e
                then Just $ Xdgdirs conf cache exe
                else Nothing
