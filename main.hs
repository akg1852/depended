{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import System.IO
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Aeson as JSON
import Text.XML.Light as XML (parseXML)
import Network.HTTP.Conduit
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- json

type JObject = HM.HashMap T.Text JSON.Value

parseJSON :: LazyChar8.ByteString -> JSON.Value
parseJSON s = case JSON.decode s of
    Just x -> x
    _ -> error "invalid json"
    
jObject :: JSON.Value -> JObject
jObject v = case v of
    JSON.Object x -> x
    _ -> error "invalid json object"
    
jArray :: JSON.Value -> Vector.Vector JSON.Value
jArray v = case v of
    JSON.Array x -> x
    _ -> error "invalid json array"

jLookupString :: T.Text -> JObject -> Maybe T.Text
jLookupString key jObject = case HM.lookup key jObject of
    Just (JSON.String v) -> Just v
    _ -> Nothing


-- config

config :: IO JObject
config = do
    file <- readFile "config.json"
    return . jObject . parseJSON . LazyChar8.pack $ file


-- repositories & projects

data Project = Project
    { projName :: T.Text
    , projRepo :: T.Text
    , projRepoBranch :: T.Text
    , projPath :: T.Text
    , projDeployable :: Maybe T.Text
    , projPackagesHash :: T.Text
    } deriving (Show, Read)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field <*> field <*> field <*> field

type RepoData = [Project]

data Relationship = Relationship
    { relParent :: T.Text
    , relChild :: T.Text
    }

instance FromRow Relationship where
    fromRow = Relationship <$> field <*> field

getRepoData :: T.Text -> T.Text -> IO RepoData
getRepoData repo branch = do
    -- connect to repo
    -- find all projects by looking for .csproj/.fsproj/.sqlproj files
    -- get project name from proj file
    -- get path to project folder
    -- check for deploy.xml, and get deployable name (and type?), if present
    -- get hash of packages.config file
    -- return with info
    return(
        [ Project
            { projName = "ABC.Foo"
            , projRepo = repo
            , projRepoBranch = branch
            , projPath = "Foo"
            , projDeployable = Just "ABC.Foo"
            , projPackagesHash = "12345"
            }
        , Project
            { projName = "ABC.Bar"
            , projRepo = repo
            , projRepoBranch = branch
            , projPath = "Bar"
            , projDeployable = Nothing
            , projPackagesHash = "54321"
            }
        ])

githubRequest :: T.Text -> IO LazyChar8.ByteString
githubRequest request = do
    c <- config
    let Just githubUrl = jLookupString "githubUrl" c
    let Just githubToken = jLookupString "githubToken" c

    r <- parseUrl . T.unpack $ T.append githubUrl request
    let request = r { method = "GET" 
                    , requestHeaders =
                        [ ("User-Agent", "depended")
                        , ("Authorization", Char8.pack ("token " ++ (T.unpack githubToken)))
                        ]
                    }
    withManager $ \manager -> do
        response <- httpLbs request manager
        return $ responseBody response


-- database

updateDB :: Project -> IO ()
updateDB projData = do
    -- update/create row in project table in db
    -- if project's packages hash is different from the one in the db
    --     get packages.config for project
    --     clear all relationship rows in db where project is parent
    --     update rows in relationship table in db from new packages file
    appendFile "fakeDB" (show projData ++ "\n")
    return ()

dbTest = do
    conn <- open "data.db"
    r <- query_ conn "select * from project" :: IO [Project]
    mapM_ print r
    close conn

-- main

main = do
    -- for each repo listed in a config file, get data
    -- update db
    -- for each project (for which we want results), get reverse-dependencies, trace them back to deployables
    -- display results
    repoData <- getRepoData "XYZ/ABC" "master"
    writeFile "fakeDB" ""
    mapM_ updateDB repoData
    output <- readFile "fakeDB"
    putStrLn output

