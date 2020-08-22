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
githubRepoURL = "https://github.com/{{reponame}}"
stackageIcon = "data:image/png;base64,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"

main = do
  resp <- getLTSInfo
  ghRepos <- getRepositories
  let lts = DT.unpack $ fromMaybe "?" $ extractLTS resp
  putStrLn $ concat [lts, "| ", "image=", stackageIcon]
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
  | lts == yamlInfo = putStrLn $ DT.unpack $ DT.concat [repoName, ": ", yamlInfo, "|color=green", " href=", repoURL repoName]
  | otherwise = putStrLn $ DT.unpack $ DT.concat [repoName, ": ", yamlInfo, "|color=red", " href=", repoURL repoName]

repoURL repo = DT.replace "{{reponame}}" repo githubRepoURL

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