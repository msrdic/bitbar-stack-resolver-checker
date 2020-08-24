#!/usr/bin/env /usr/local/bin/stack
{- stack
  --resolver lts-16.10
  --install-ghc
  runghc
  --package wreq
  --package aeson
  --package lens-aeson
-}

-- <bitbar.title>Github projects stack resolver checker</bitbar.title>
-- <bitbar.version>v0.1</bitbar.version>
-- <bitbar.author>Mladen Srdić</bitbar.author>
-- <bitbar.author.github>msrdic</bitbar.author.github>
-- <bitbar.desc>
--     This plugin shows stack resolver versions for your public Haskell/Stack projects.
-- </bitbar.desc>
-- <bitbar.dependencies>haskell, stack</bitbar.dependencies>

{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad           (forM_)
import           Data.Aeson.Lens
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Vector             as V
import qualified Network.HTTP.Client     as HTTP
import           Network.Wreq

main = do
  resolversResponse <- getLTSInfo
  let lts = T.unpack $ fromMaybe "?" $ extractLTS resolversResponse
  putStrLn lts
  putStrLn "---"
  ghReposResp <- getPublicHaskellRepositories
  let repos = extractRepositories ghReposResp
      repoNames = extractRepoNames repos
      maxNameLen = longestRepoName repoNames
  forM_ repoNames (printSingleRepo lts maxNameLen)

-- stack resolvers section
resolversURL = "https://www.stackage.org/download/lts-snapshots.json"
getLTSInfo = getWith defaults resolversURL
extractLTS j = j ^. responseBody ^? key "lts" . _String

-- github section
username = "msrdic"
searchRepositoriesURL =
  "https://api.github.com/search/repositories?q=language:haskell+user:" ++ username
rawPath = "https://raw.githubusercontent.com/{{reponame}}/master/stack.yaml"
githubRepoURL = "https://github.com/{{reponame}}"

getPublicHaskellRepositories = getWith defaults searchRepositoriesURL

extractRepositories j = j ^. responseBody ^. key "items" . _Array

extractRepoNames = V.toList . V.map extractRepoName
extractRepoName j = j ^. key "full_name" . _String

longestRepoName = Prelude.maximum . Prelude.map T.length

printSingleRepo lts maxNameLen repoName = do
  repoInfo <- getRepoState repoName
  printRepoInfo (T.pack lts) maxNameLen repoInfo

getRepoState repoName = do
  yamlResp <- getRawStackYaml repoName
  let statusCode = extractStatusCode yamlResp
  case statusCode of
    200 -> let resolverVersion = extractResolverVersion $ bodyAsText yamlResp
           in return (repoName, resolverVersion)
    _   -> return (repoName, "?")

getRawStackYaml repoName = do
  let yamlPath = toRawYamlPath rawPath repoName
  getWith (defaults & ignoreNon200) $ T.unpack yamlPath

toRawYamlPath path repoName = T.replace "{{reponame}}" repoName path
ignoreNon200 = checkResponse .~ Just (\_ _ -> return ())

extractStatusCode j = j ^. responseStatus . statusCode
bodyAsText = toStrict . decodeUtf8 . HTTP.responseBody

printRepoInfo lts maxRepoNameLen (repoName, yamlInfo)
  | lts == yamlInfo = putStrLn $ T.unpack $ T.concat [repoName, repoPadding, " ", resolverPadding, yamlInfo, "|color=green", " href=", repoURL repoName, " font=Courier New"]
  | otherwise       = putStrLn $ T.unpack $ T.concat [repoName, repoPadding, " ", resolverPadding, yamlInfo, "|color=red", " href=", repoURL repoName, " font=Courier New"]
  where k = maxRepoNameLen - T.length repoName
        p = T.length lts - T.length yamlInfo
        repoPadding = T.replicate k " "
        resolverPadding = T.replicate p " "

repoURL repo = T.replace "{{reponame}}" repo githubRepoURL

extractResolverVersion rb = extractResolverVersion' (T.lines rb)
extractResolverVersion' [] = "?"
extractResolverVersion' (l:ls) | isCommentLine l  = extractResolverVersion' ls
                               | isResolverLine l = extractResolverValue l
                               | otherwise        = extractResolverVersion' ls

isCommentLine = ("#" `T.isPrefixOf`)
isResolverLine = ("resolver" `T.isPrefixOf`)

extractResolverValue = T.strip . T.drop 1 . T.dropWhile (/= ':')
