{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import System.IO
import Data.Maybe
import Data.List
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO (putStrLn)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import qualified Data.Aeson as JSON
import qualified Text.XML.Light as XML
import Network.HTTP.Conduit
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField

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
    
jArray :: JSON.Value -> V.Vector JSON.Value
jArray v = case v of
    JSON.Array x -> x
    _ -> error "invalid json array"

jString :: JSON.Value -> T.Text
jString v = case v of
    JSON.String x -> x
    _ -> error "invalid json string"

jLookup :: T.Text -> JObject -> Maybe JSON.Value
jLookup key jObject = HM.lookup key jObject


-- xml

parseXML :: LazyChar8.ByteString -> XML.Element
parseXML s = case XML.parseXMLDoc s of
    Just x -> x
    _ -> error "invalid xml"


-- config

config :: IO JObject
config = do
    file <- readFile "config.json"
    return . jObject . parseJSON . LazyChar8.pack $ file


-- projects and relationships

data Project = Project
    { projName :: T.Text
    , projRepo :: T.Text
    , projRepoBranch :: T.Text
    , projPath :: T.Text
    , projDeployable :: Maybe T.Text
    , projPackagesHash :: Maybe T.Text
    } deriving (Show, Read)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Project where
    toRow (Project a b c d e f) = [toField a, toField b, toField c, toField d, nullable e, nullable f]
        where nullable x = if isJust x then toField $ fromJust x else SQLNull

data Relationship = Relationship
    { relParent :: T.Text
    , relChild :: T.Text
    }

instance FromRow Relationship where
    fromRow = Relationship <$> field <*> field


-- github

githubRequest :: T.Text -> IO LazyChar8.ByteString
githubRequest request = do
    c <- config
    let Just githubUrl = jLookup "githubUrl" c
    let Just githubToken = jLookup "githubToken" c

    r <- parseUrl . T.unpack $ T.append (jString githubUrl) request
    let request = r { method = "GET" 
                    , requestHeaders =
                        [ ("User-Agent", "depended")
                        , ("Authorization", Char8.pack ("token " ++ (T.unpack $ jString githubToken)))
                        ]
                    }
    withManager $ \manager -> do
        response <- httpLbs request manager
        return $ responseBody response


-- database

deleteProject :: T.Text -> IO ()
deleteProject name = do
    conn <- open "data.db"
    execute conn "delete from project where name like ?" (Only name)
    execute conn "delete from relationship where parent like ?" (Only name)
    close conn

insertProject :: Project -> IO ()
insertProject project = do
    conn <- open "data.db"
    execute conn "insert into project values (?, ?, ?, ?, ?, ?)" project
    close conn

insertRelationship :: T.Text -> T.Text -> IO()
insertRelationship parent child = do
    conn <- open "data.db"
    execute conn "insert into relationship values (?, ?)" [parent, child]
    close conn

selectReverseDependencies :: T.Text -> IO [T.Text]
selectReverseDependencies s = do
    conn <- open "data.db"
    rels <- query conn "select * from relationship where child = ?" (Only s)
    close conn
    return $ map relParent rels

selectAllProjects :: IO [T.Text]
selectAllProjects = do
    conn <- open "data.db"
    projs <- query_ conn "select * from project"
    close conn
    return $ map projName projs


-- main

main = do
    deleteProject "%"
    c <- config
    let Just repos = jLookup "repositories" c
    Foldable.forM_ (fmap jObject $ jArray repos) $ \r -> do
        let Just repo = jLookup "repo" r
        let Just branch = jLookup "branch" r
        projects <- githubRequest $ T.concat ["/repos/", jString repo, "/contents/project?ref=", jString branch]
        Foldable.forM_ (V.filter isDir . fmap jObject . jArray $ parseJSON projects) $ \p -> do
            let Just name = jLookup "name" p
            proj <- githubRequest $ T.concat ["/repos/", jString repo, "/contents/project/", jString name, "?ref=", jString branch]
            let files = V.filter (not . isDir) . fmap jObject . jArray $ parseJSON proj
            let csproj = V.find (\f -> fmap jString (jLookup "name" f) == Just (T.append (jString name) ".csproj")) files
            if isJust csproj
                then do
                    let packages = V.find (\f -> fmap jString (jLookup "name" f) == Just ("packages.config")) files
                    if isJust packages
                        then do
                            let Just packagesUrl = jLookup "download_url" (fromJust packages)
                            packageXml <- simpleHttp . T.unpack $ jString packagesUrl
                            let packageEls = XML.findChildren (XML.unqual "package") $ parseXML packageXml
                            let packageDeps = map (fromJust . XML.findAttr (XML.unqual "id")) packageEls
                            forM_ packageDeps $ \d -> insertRelationship (jString name) (T.pack d)
                        else deleteProject $ jString name

                    let Just csprojUrl = jLookup "download_url" (fromJust csproj)
                    csprojXml <- simpleHttp . T.unpack $ jString csprojUrl
                    let pRefEls = XML.filterElementsName (\qn -> let n = XML.qName qn in n == "ProjectReference") $ parseXML csprojXml
                    let csprojDeps = map (XML.strContent . fromJust . XML.filterChildName (\qn -> let n = XML.qName qn in n == "Name")) pRefEls
                    forM_ csprojDeps $ \d -> insertRelationship (jString name) (T.pack d)

                    let hash = if isJust packages then jLookup "sha" $ fromJust packages else Nothing
                    let project = Project { projName = jString name
                                          , projRepo = jString repo
                                          , projRepoBranch = jString branch
                                          , projPath = jString name
                                          , projDeployable = Nothing -- todo: get deployable data
                                          , projPackagesHash = fmap jString hash
                                          }
                    insertProject project
                else deleteProject $ jString name

    allProjects <- selectAllProjects
    forM_ allProjects $ \p -> do
        rDeps <- selectReverseDependencies p 
        T.IO.putStrLn p
        print $ nub rDeps
        putStrLn ""
  where

    isDir o = let Just t = jLookup "type" o in jString t == "dir" 
