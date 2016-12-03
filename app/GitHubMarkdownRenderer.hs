{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Network.Wreq

main = do
  content <- readFile "e.md"
  let params = object [
                 "text" .= content
               ]
  r <- post "https://api.github.com/markdown" (toJSON params)
  putStr $ BSLU.toString $ r ^. responseBody

