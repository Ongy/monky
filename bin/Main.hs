{-
    Copyright 2015,2016 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE CPP #-}
{-|
Module      : monky
Description : A conky clone
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This application has a subset of the functionality conky provides.

The goal of this application is to provide a standalone access to most system
values without spawning thousands of processes (looking at you conky).

It uses a similar configuration style to xmonad, i.e. the config file is a
haskell source file which uses library functions provided by the application.

The config file has to be placed at "~/.monky/monky.hs". Any valid Haskell
source file should work (it is compiled with ghc --make).

This executable compiles the configuration if needed, and execs into the main
executable.
-}
module Main
  ( main
  )
where

import Debug.Trace

import qualified Xdg

import Monky.Version (getVersion)
import Control.Monad (when, void)
import Data.List (nub, sort)
import System.Directory
import System.Exit (ExitCode(..), exitFailure)
import System.IO (withFile, IOMode(..), hPutStr, hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Process (system)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import System.IO.Error (tryIOError)

import Options.Applicative

data Action
  = Create
  | Clean
  | Recompile
  | Execute
  deriving (Ord, Show, Eq)

getActions :: Config -> [Action]
getActions c = catMaybes
    [ if confClean c then Just Clean else Nothing
    , if confGenerate c then Just Create else Nothing
    , if confNoCompile c then Nothing else Just Recompile
    , if confNoExec c then Nothing else Just Execute
    ]

data Config = Config
  { _monkyDir :: String
  , _forceXdg :: Bool

  , confClean :: Bool
  , confNoCompile :: Bool

  , confNoExec :: Bool

  , confGenerate :: Bool
  , confNoGenerate :: Bool
  } deriving (Show)


data Paths = Paths
    { configFile :: FilePath
    , exeFile    :: FilePath
    , cacheDir   :: FilePath
    } deriving (Show)

getConfigP :: Parser Config
getConfigP = Config <$>
   strOption (long "monky-dir" <> help "The directory monky resides in. Defaults to ~/.monky" <> short 'd' <> value "") <*>
   switch (long "xdg-dirs" <> help "Force xdg mode" <> short 'x') <*>
   switch (long "compile" <> help "Remove temporary files, this will force a compilation step" <> short 'c') <*>
   switch (long "no-compile" <> help "Do not try to compile the executable" <> short 'n') <*>
   switch (long "no-exec" <> help "Do not execute the compiled executable") <*>
   switch (long "generate-example" <> help "Force example generation. This will overwrite existing config!") <*>
   switch (long "no-generate-example" <> help "Don't generate the example, even if required")


monkyDesc :: String
monkyDesc = concat
  [ "Monky version: " ++ show getVersion ++ "."
  , "Monky helper to compile and/or execute the real monky binary"
  , "This executable will call ghc to (re)compile the main monky.exe"
  , "Then it will exec monky.exe which in turn will generate the output"
  , "This file is a simple helper/wrapper. You can execute monky.exe without."
  ]


getConfig :: IO Config
getConfig = do
    conf <- execParser $ info (helper <*> getConfigP) (fullDesc <> header monkyDesc)

    if (confGenerate conf && confNoGenerate conf)
        then do
            hPutStrLn stderr "gernate and no-generate given as arguments, are you trying to fool me?"
            exitFailure
        else return conf


compilerFlags :: Paths -> String
compilerFlags p =
    let cdir = cacheDir p
        sdir = reverse . dropWhile (/= '/') . reverse $ configFile p
     in concat ["--make -XOverloadedStrings -outputdir ", cdir, " -i", sdir, " -O -with-rtsopts=-V0 "]


exampleFile :: String
exampleFile = unlines
     [ "import Monky"
     , "import Monky.Modules"
     , ""
     , "import Monky.Examples.CPU"
     , "import Monky.Examples.Memory"
     , ""
     , "import Monky.Outputs.Ascii"
     , ""
     , "main :: IO ()"
     , "main = startLoop getAsciiOut"
     , "  [ pollPack 1 $ getRawCPU"
     , "  , pollPack 1 $ getMemoryHandle"
     , "  ]"
     ]


createExample :: Paths -> IO ()
createExample p = do
    let dir = reverse . dropWhile (/= '/') . reverse $ configFile p
    createDirectoryIfMissing False dir
    withFile (configFile p) WriteMode (`hPutStr` exampleFile)


shouldCreate :: Paths -> IO Bool
shouldCreate p = do
    exe <- doesFileExist (exeFile p)
    conf <- doesFileExist (configFile p)
    return $ not (exe || conf)


compile :: Paths -> IO ()
compile p = do
    exists <- doesFileExist (configFile p)
    createDirectoryIfMissing False (cacheDir p)
    when exists $ do
        ret <- system $ traceShowId $ concat ["ghc ", compilerFlags p, configFile p, " -o ", exeFile p]
        case ret of
            (ExitFailure _) -> do
                    hPutStrLn stderr "Compilation failed"
                    exitFailure
            ExitSuccess -> return ()


forceRecomp :: Paths -> IO ()
forceRecomp p = do
    void . tryIOError . removeFile . exeFile $ p
    void . tryIOError . removeDirectoryRecursive . cacheDir $ p


executeMonky :: Paths -> IO ()
executeMonky p = executeFile (exeFile p) False [] Nothing


execAction :: Action -> (Paths -> IO ())
execAction Create = createExample
execAction Clean = forceRecomp
execAction Recompile = compile
execAction Execute = executeMonky

singlePaths :: FilePath -> Paths
singlePaths dir = Paths (dir ++ "/monky.hs") (dir ++ "/monky.exe") (dir ++ "/build/")

getPaths :: Config -> IO Paths
getPaths (Config "" f _ _ _ _ _) = do
    xdg <- Xdg.getXdgDirs f
    case xdg of
        Nothing -> singlePaths . (++ "/.monky/") <$> getHomeDirectory
        Just (Xdg.Xdgdirs conf exe cache) -> return $ Paths conf exe cache
getPaths (Config x _ _ _ _ _ _) =
    return . singlePaths $ x

main :: IO ()
main = do
    setCurrentDirectory "/"
    conf <- getConfig
    paths <- getPaths conf
    should <- shouldCreate paths
    let actions = getActions conf
    let acts = map execAction . nub . sort $ if should && not (confNoGenerate conf) then Create:actions else actions
    mapM_ ($ paths) acts
