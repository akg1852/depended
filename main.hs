{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import System.IO
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Aeson as JSON
import Text.XML.Light as XML (parseXML)
import Network.HTTP.Conduit

-- json

type JObject = Map.Map String JSON.Value

parseJSON :: LazyChar8.ByteString -> JObject
parseJSON json = case JSON.decode json of
    Just x -> x
    Nothing -> error "invalid json object"
parseJSONstring = parseJSON . LazyChar8.pack

jLookupString :: String -> JObject -> Maybe String
jLookupString key jObject = case Map.lookup key jObject of
    Just (JSON.String v) -> Just (Text.unpack v)
    _ -> Nothing


-- config

config :: IO JObject
config = do
    file <- readFile "config.json"
    return $ parseJSONstring file


-- repositories & projects

data ProjData = ProjData
    { projName :: String
    , projRepo :: String
    , projRepoBranch :: String
    , projPath :: String
    , projDeployable :: String
    , projPackagesHash :: String
    } deriving (Show, Read)
type RepoData = [ProjData]

getRepoData :: String -> String -> IO RepoData
getRepoData repo branch = do
    -- connect to repo
    -- find all projects by looking for .csproj/.fsproj/.sqlproj files
    -- get project name from proj file
    -- get path to project folder
    -- check for deploy.xml, and get deployable name (and type?), if present
    -- get hash of packages.config file
    -- return with info
    return(
        [ ProjData
            { projName="ABC.Foo"
            , projRepo=repo
            , projRepoBranch=branch
            , projPath="Foo"
            , projDeployable="ABC.Foo"
            , projPackagesHash="12345"
            }
        , ProjData
            { projName="ABC.Bar"
            , projRepo=repo
            , projRepoBranch=branch
            , projPath="Bar"
            , projDeployable=""
            , projPackagesHash="54321"
            }
        ])

githubRequest :: String -> IO JObject
githubRequest request = do
    c <- config
    let Just githubUrl = jLookupString "githubUrl" c
    let Just githubToken = jLookupString "githubToken" c

    r <- parseUrl (githubUrl ++ request)
    let request = r { method = "GET" 
                    , requestHeaders =
                        [ ("User-Agent", "depended")
                        , ("Authorization", Char8.pack ("token " ++ githubToken))
                        ]
                    }
    withManager $ \manager -> do
        response <- httpLbs request manager
        return . parseJSON $ responseBody response


-- database

updateDB :: ProjData -> IO ()
updateDB projData = do
    -- update/create row in project table in db
    -- if project's packages hash is different from the one in the db
    --     get packages.config for project
    --     clear all relationship rows in db where project is parent
    --     update rows in relationship table in db from new packages file
    appendFile "fakeDB" (show projData ++ "\n")
    return ()


-- main

main = do
    -- for each repo listed in a config file, get data
    -- update db
    -- for each project (for which we want results), get reverse-dependencies, trace them back to deployables
    -- display results
    repoData <- getRepoData "git@github.server.com:XYZ/ABC.git" "master"
    writeFile "fakeDB" ""
    mapM_ updateDB repoData
    output <- readFile "fakeDB"
    putStrLn output

