{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (dropWhileEnd, intercalate, isInfixOf, nub, sort, stripPrefix,
                  (\\))
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.Wreq
import Text.Regex
import System.Directory
import System.Environment

main = do
  args <- getArgs
  crawl (args !! 0) ["/index.html"]

crawl targetDir pathQueue =
  crawl' targetDir pathQueue []
  where crawl' targetDir [] _ = do
          return ()
        crawl' targetDir pathQueue@(path:xs) urlDone = do
          putStrLn $ "VISIT " ++ path
          let base = dropWhileEnd (/='/') path
          body <- handle errorHandler $ do
                           r <- get ("http://book.realworldhaskell.org" ++ path)
                           return $ r ^. responseBody
          unless (BSL.null body)
                 (write (targetDir ++ path) body) -- TODO: use path
          let validPaths = unique . fromMaybe [] . sequence .
                           filterValid base . search . BSLU.toString $ body
          let newPaths = validPaths \\ (pathQueue ++ urlDone)
          crawl' targetDir (newPaths ++ xs) (path:urlDone)
        filterValid base = filter isJust . map (toValid base)
        unique = nub . sort
        errorHandler e@(StatusCodeException s _ _)
          | s ^. statusCode == 404 = return ""
          | otherwise              = error (show e)

toValid base p =
  let excludedSchemes = ["http://", "https://", "mailto:"]
      woFragment = takeWhile (/='#') p
      chkSelfHost = stripPrefix "http://book.realworldhaskell.org" woFragment
      p' = fromMaybe woFragment chkSelfHost
  in if length p' == 0 ||
        any (hasPrefix p') excludedSchemes ||
        "/buy/" `isPrefixOf` p'
     then Nothing
     else if p' !! 0 == '/'
          then Just p'
          else Just $ base ++ p'

search s =
  let patterns = ["<link[^>]*href=\"([^\"]*)\"[^>]*>",
                  "<script[^>]*src=\"([^\"]*)\"[^>]*>",
                  "<a[^>]*href=\"([^\"]*)\"[^>]*>",
                  "<img[^>]*src=\"([^\"]*)\"[^>]*>",
                  "url\\(([^)]*)\\);"]
                 -- POSIX regex does not have non-greedy quantifiers.
      concatPatterns = intercalate "|"
      regex = mkRegex $ concatPatterns patterns
  in matchAll regex s

matchAll regex s =
  reverse $ matchAll' regex s []
  where matchAll' regex s acc =
          case matchRegexAll regex s of
            Just (_, _, s', m) -> matchAll' regex s' (concat m:acc)
            Nothing -> acc

write fullPath content = do
  let base = dropWhileEnd (/='/') fullPath
  createDirectoryIfMissing True base
  let index = if last fullPath == '/'
              then if isInfixOf "/feeds/comments" fullPath
                   then "index.xml"
                   else "index.html"
              else ""
  BSL.writeFile (fullPath ++ index) content

isPrefixOf p s =
  isJust $ stripPrefix p s
hasPrefix s p =
  isJust $ flip stripPrefix s p

