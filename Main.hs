#!/usr/bin/env /usr/local/bin/stack
{- stack
  --resolver lts-16.10
  --install-ghc
  runghc
  --package wreq
  --package aeson
  --package lens-aeson
-}

-- <bitbar.title>Stackage LTS monitor</bitbar.title>
-- <bitbar.version>v0.1</bitbar.version>
-- <bitbar.author>Mladen Srdić</bitbar.author>
-- <bitbar.author.github>msrdic</bitbar.author.github>
-- <bitbar.desc>
--   This plugin simply shows the latest LTS Stackage version in your menubar.
-- </bitbar.desc>
-- <bitbar.dependencies>haskell, stack</bitbar.dependencies>

{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as DT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import           Network.Wreq
import qualified Network.HTTP.Client as HTTP
import qualified Data.Vector        as DV
import           Control.Monad.IO.Class        (liftIO)


listLTSPath = "https://www.stackage.org/download/lts-snapshots.json"
username :: DT.Text
username = "msrdic"
searchRepositoriesURL =
  "https://api.github.com/search/repositories?q=language:haskell+user:" ++ (DT.unpack username)
rawPath = "https://raw.githubusercontent.com/{{reponame}}/master/stack.yaml"

main = do
  resp <- getLTSInfo
  ghRepos <- getRepositories
  let lts = DT.unpack $ fromMaybe "?" $ extractLTS resp
  putStrLn lts
  putStrLn "---"
  let repos = extractRepositories ghRepos
      repoNames = DV.toList $ DV.map extractRepoName repos
  repoInfos <- mapM getRepoState repoNames
  mapM_ (printRepoInfo (DT.pack lts)) repoInfos

getRepoState repoName = do
  yaml <- getRawStackYaml repoName
  let statusCode = extractStatusCode yaml
  case statusCode of
    200 -> return $ (repoName, extractResolverLine $ toStrict $ decodeUtf8 $ HTTP.responseBody yaml)
    _   -> return $ (repoName, "Unknown")

printRepoInfo lts (repoName, yamlInfo)
  | lts == yamlInfo = putStrLn $ DT.unpack $ DT.concat [repoName, ": ", yamlInfo, "|color=green"]
  | otherwise = putStrLn $ DT.unpack $ DT.concat [repoName, ": ", yamlInfo, "|color=red"]

extractStatusCode j = j ^. responseStatus . statusCode

extractResolverLine rb =
  let ls = DT.lines rb
  in extractResolverLine' ls

extractResolverLine' [] = "?"
extractResolverLine' (l:ls) | DT.isPrefixOf "#" l = extractResolverLine' ls
                            | DT.isPrefixOf "resolver" l = extractResolverValue l
                            | otherwise = extractResolverLine' ls

extractResolverValue l = 
  let v1 = DT.dropWhile (\c -> c /= ':') l
      v2 = DT.drop 1 v1
  in DT.dropAround (\c -> c == ' ') v2

extractLTS j = j ^. responseBody ^? key "lts" . _String
extractRepositories j = j ^. responseBody ^. key "items" . _Array
extractRepoName j = j ^. key "full_name" . _String

getLTSInfo = getWith defaults $ listLTSPath
getRepositories = getWith defaults $ searchRepositoriesURL
getRawStackYaml repoName = do
  let yamlPath = toRawYamlPath rawPath repoName
  getWith (defaults & checkResponse .~ Just (\_ _ -> return ()))
    $ DT.unpack yamlPath

toRawYamlPath :: DT.Text -> DT.Text -> DT.Text
toRawYamlPath path repoName = 
  DT.replace "{{reponame}}" repoName path