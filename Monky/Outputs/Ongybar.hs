{-
    Copyright 2017 Markus Ongyerth

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
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Outputs.Ongybar
Description : Output module for Ongybar
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for piping into a ongybar
-}
module Monky.Outputs.Ongybar
  ( OngyOutput
  , getOngyOut
  )
where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Char8 as BS
import Monky.Modules

import Data.Serialize

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Read as R

import Control.Applicative (pure)

-- |The output handle for dzen2 pipe
data OngyOutput = OngyOutput


putString :: Text -> Put
putString t = do
    let bs =  E.encodeUtf8 t
    putWord16host . fromIntegral $ BS.length bs
    putByteString bs
    putWord8 0

putColor :: Text -> Put
putColor t = if T.length t == 0
    then do
        putWord8 0xFF
        putWord8 0xFF
        putWord8 0xFF
        putWord8 0xFF
    else do
        let (Right (red  , _)) = R.hexadecimal . T.take 2 . T.drop 1 $ t
        let (Right (green, _)) = R.hexadecimal . T.take 2 . T.drop 3 $ t
        let (Right (blue , _)) = R.hexadecimal . T.take 2 . T.drop 5 $ t

        putWord8 $ red
        putWord8 $ green
        putWord8 $ blue
        putWord8 0xFF

putElem :: MonkyOut -> Put
putElem (MonkyPlain t) = do
    putWord8 1
    putString t
putElem (MonkyImage t _) = do
    putWord8 2
    putString $ "/home/ongy/.local/share/monky/xbm/" `T.append` t `T.append` ".bmp"
putElem (MonkyColor (f, b) o) = do
    putWord8 3
    putColor f
    putColor b
    putElem o
putElem (MonkyBar p) = do
    putWord8 4
    -- Width first
    putWord16host 25
    putWord16host . fromIntegral $ p
putElem (MonkyHBar p) = do
    putWord8 4
    putWord16host . fromIntegral $ p * 10
    putWord16host 50

isDraw :: MonkyOut -> Bool
isDraw (MonkyHBar _) = True
isDraw (MonkyColor _ x) = isDraw x
isDraw _ = False

-- The monky element to output, and an offset
putDraws :: [MonkyOut] -> Float -> PutM Float
putDraws [] f = pure f
putDraws ((MonkyHBar x):xs) f = do
    let right = fromIntegral x + f
    putWord8 1
    putWord16host . floor $ f -- x1
    putWord16host 25 -- y1
    putWord16host . floor $ right -- x2
    putWord16host 75 -- y2
    putDraws xs right
putDraws ((MonkyColor (c, _) x):xs) f = do
    putWord8 3
    putColor c
    f2 <- putDraws [x] f
    putDraws xs f2
putDraws (_:xs) f = putDraws xs f

groupDrawable :: [MonkyOut] -> Put
groupDrawable xs = do
    -- ys is a group of drawables!
    let ys = dropWhile (not . isDraw) xs
    let zs = dropWhile (isDraw) ys

    putWord8 0
    putWord8 3
    putList $ takeWhile (not . isDraw) xs
    putWord8 5
    putWord8 2 -- Draw in semi-absolute values
    putWord8 . fromIntegral $ length ys
    _ <- putDraws ys 0

    putList $ zs

putList :: [MonkyOut] -> Put
putList xs = if any isDraw xs
    then groupDrawable xs
    else  do
        putWord8 0
        putWord8 . fromIntegral $ length xs
        mapM_ putElem xs


instance MonkyOutput OngyOutput where
    doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
    doLine _ xs = do
        BS.hPutStr stdout . runPut $ do
            putWord8 . fromIntegral $ length xs
            mapM_ putList xs
        hFlush stdout

-- |Get an output handle for dzen2 formatting
-- Assumes @" | "@ as divider
getOngyOut :: IO OngyOutput
getOngyOut = pure OngyOutput
