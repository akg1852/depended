{-# LANGUAGE OverloadedStrings #-}

module Model where

import System.Environment.FindBin
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField

import Parse


-- config

config :: IO JObject
config = do
    dir <- getProgPath
    file <- readFile (dir ++ "/config.json")
    return . jObject . parseJSON . LazyChar8.pack $ file


-- projects and relationships

data Project = Project
    { projName :: T.Text
    , projRepo :: T.Text
    , projRepoBranch :: T.Text
    , projDeployable :: T.Text
    , projProjHash :: T.Text
    , projPackagesHash :: Maybe T.Text
    } deriving (Show, Read)

instance FromRow Project where
    fromRow = Project <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Project where
    toRow (Project a b c d e f) = [toField a, toField b, toField c, toField d, toField e, nullable f]
        where nullable x = if isJust x then toField $ fromJust x else SQLNull

data Relationship = Relationship
    { relParent :: T.Text
    , relChild :: T.Text
    }

instance FromRow Relationship where
    fromRow = Relationship <$> field <*> field


-- database

openDb :: IO Connection
openDb = do
    dir <- getProgPath
    c <- config
    conn <- open . (dir ++) . ("/" ++) . T.unpack . jString . fromJust $ jLookup "dbFile" c
    return conn

deleteProject :: T.Text -> IO ()
deleteProject name = do
    conn <- openDb
    execute conn "DELETE FROM project WHERE name LIKE ?" (Only name)
    execute conn "DELETE FROM relationship WHERE parent LIKE ?" (Only name)
    close conn

insertProject :: Project -> IO ()
insertProject project = do
    conn <- openDb
    execute conn "INSERT INTO project VALUES (?, ?, ?, ?, ?, ?)" project
    close conn

insertRelationship :: T.Text -> T.Text -> IO()
insertRelationship parent child = do
    conn <- openDb
    execute conn "INSERT INTO relationship VALUES (?, ?)" [parent, child]
    close conn

selectReverseDependencies :: T.Text -> IO [T.Text]
selectReverseDependencies projName = do
    conn <- openDb
    rels <- query conn "SELECT * FROM relationship WHERE child = ? GROUP BY parent, child ORDER BY parent" (Only projName)
    close conn
    return $ map relParent rels

selectProjects :: T.Text -> IO [T.Text]
selectProjects search = do
    conn <- openDb
    projs <- query conn "SELECT * FROM project WHERE name LIKE ? ORDER BY name" (Only search )
    close conn
    return $ map projName projs

selectAllProjects :: IO [T.Text]
selectAllProjects = selectProjects "%"

selectProjectField :: (Project -> a) -> T.Text -> IO (Maybe a)
selectProjectField field projName = do
    conn <- openDb
    projs <- query conn "SELECT * FROM project WHERE name = ?" (Only projName)
    close conn
    return $ if null projs then Nothing else Just . field $ head projs

updateProjectDeployable :: T.Text -> Bool -> IO ()
updateProjectDeployable projName v = do
    conn <- openDb
    execute conn "UPDATE project SET deployable = ? WHERE name = ?" [T.pack $ show v, projName]
    close conn
