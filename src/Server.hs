{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop (writeBS "hi")
