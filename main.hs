{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import System.IO
import Data.Maybe
import Data.List
import System.Environment
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
import System.Console.ANSI


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

jArrayOfObject :: JSON.Value -> V.Vector JObject
jArrayOfObject = fmap jObject . jArray

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

qNameIs :: T.Text -> (XML.QName -> Bool)
qNameIs n q = let qn = T.pack (XML.qName q) in qn == n


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


-- github

githubRequest :: T.Text -> IO LazyChar8.ByteString
githubRequest request = do
    c <- config
    let githubUrl = jString . fromJust $ jLookup "githubUrl" c
    let githubToken = jString . fromJust $ jLookup "githubToken" c

    r <- parseUrl . T.unpack $ T.append githubUrl request
    let request = r { method = "GET" 
                    , requestHeaders =
                        [ ("User-Agent", "depended")
                        , ("Authorization", Char8.pack ("token " ++ (T.unpack $ githubToken)))
                        ]
                    }
    withManager $ \manager -> do
        response <- httpLbs request manager
        return $ responseBody response

getData :: IO ()
getData = do
    c <- config
    let repos = jArrayOfObject . fromJust $ jLookup "repositories" c
    Foldable.forM_ repos $ \r -> do
        let repo = jString . fromJust $ jLookup "repo" r
        let branch = jString . fromJust $ jLookup "branch" r
        projects <- githubRequest $ T.concat ["/repos/", repo, "/contents/project?ref=", branch]
        Foldable.forM_ (V.filter isDir . jArrayOfObject $ parseJSON projects) $ \p -> do
            let name = jString . fromJust $ jLookup "name" p
            proj <- githubRequest $ T.concat ["/repos/", repo, "/contents/project/", name, "?ref=", branch]
            let files = V.filter (not . isDir) . jArrayOfObject $ parseJSON proj

            let csproj = join . find isJust $ map (($ files) . V.find . jNameIs . projFile name) ["cs", "fs", "sql"]
            if isNothing csproj then deleteProject $ name
            else do
                let packages = V.find (jNameIs "packages.config") files

                oldPackagesHash <- selectProjectField projPackagesHash name
                let packagesHash = if isJust packages then jLookup "sha" $ fromJust packages else Nothing
                let isNewPackagesHash = join oldPackagesHash /= fmap jString packagesHash

                oldProjHash <- selectProjectField projProjHash name
                let projHash = jLookup "sha" $ fromJust csproj
                let isNewProjHash = oldProjHash /= fmap jString projHash

                when (isNewPackagesHash || isNewProjHash) $ do
                    deleteProject $ name
                    insertProject $ Project { projName = name
                                            , projRepo = repo
                                            , projRepoBranch = branch
                                            , projDeployable = "False"
                                            , projProjHash = jString $ fromJust projHash
                                            , projPackagesHash = fmap jString packagesHash
                                            }
                    when (isNewPackagesHash && isJust packages) $
                        getDependencies name (fromJust packages) "package" (XML.findAttrBy (qNameIs "id"))
                    when isNewProjHash $
                        (getDependencies name (fromJust csproj) "ProjectReference"
                        (fmap XML.strContent . XML.filterChildName (qNameIs "Name")))

                updateProjectDeployable name $ V.any (jNameIs "deploy.xml") files
  where
    isDir o = (jString . fromJust $ jLookup "type" o) == "dir"
    projFile name ext = T.concat [name, ".", ext, "proj"]
    jNameIs name o = (jString . fromJust $ jLookup "name" o) == name

getDependencies :: T.Text -> JObject -> T.Text -> (XML.Element -> Maybe String) -> IO ()
getDependencies projName file elName extractDepName = do
    xml <- simpleHttp . T.unpack . jString . fromJust $ jLookup "download_url" file
    let els = XML.filterElementsName (qNameIs elName) $ parseXML xml
    let deps = fmap (T.pack . fromJust . extractDepName) els
    mapM_ (insertRelationship projName) deps


-- database

openDb :: IO Connection
openDb = do
    c <- config
    conn <- open . T.unpack . jString . fromJust $ jLookup "dbFile" c
    return conn

deleteProject :: T.Text -> IO ()
deleteProject name = do
    conn <- openDb
    execute conn "delete from project where name like ?" (Only name)
    execute conn "delete from relationship where parent like ?" (Only name)
    close conn

insertProject :: Project -> IO ()
insertProject project = do
    conn <- openDb
    execute conn "insert into project values (?, ?, ?, ?, ?, ?)" project
    close conn

insertRelationship :: T.Text -> T.Text -> IO()
insertRelationship parent child = do
    conn <- openDb
    execute conn "insert into relationship values (?, ?)" [parent, child]
    close conn

selectReverseDependencies :: T.Text -> IO [T.Text]
selectReverseDependencies projName = do
    conn <- openDb
    rels <- query conn "select * from relationship where child = ? group by parent, child order by parent" (Only projName)
    close conn
    return $ map relParent rels

selectAllProjects :: IO [T.Text]
selectAllProjects = do
    conn <- openDb
    projs <- query_ conn "select * from project order by name"
    close conn
    return $ map projName projs

selectProjectField :: (Project -> a) -> T.Text -> IO (Maybe a)
selectProjectField field projName = do
    conn <- openDb
    projs <- query conn "select * from project where name = ?" (Only projName)
    close conn
    return $ if null projs then Nothing else Just . field $ head projs

updateProjectDeployable :: T.Text -> Bool -> IO ()
updateProjectDeployable projName v = do
    conn <- openDb
    execute conn "update project set deployable = ? where name = ?" [T.pack $ show v, projName]
    close conn


-- deployable reverse dependencies

getDeployableReverseDependencies :: T.Text -> IO [T.Text]
getDeployableReverseDependencies proj = (go [proj]) . (delete proj) =<< selectAllProjects
  where
    go [] _ = return []
    go candidates pool = do
        deployables <- filterM isDeployable candidates
        revDeps <- mapM selectReverseDependencies candidates
        let newCandidates = intersect (nub pool) (nub $ concat revDeps)
        let newPool = (nub pool) \\ newCandidates
        remains <- go newCandidates newPool
        return $ deployables ++ remains
    isDeployable proj = do
        d <- selectProjectField projDeployable proj
        return $ d == Just "True"


-- main

main = do
    args <- getArgs
    when ("--clear" `elem` args) $ deleteProject "%"
    when ("--cached" `notElem` args) getData
    printAll ("--immediate" `elem` args)


-- display

printAll :: Bool -> IO ()
printAll isImmediate = do
    allProjects <- selectAllProjects
    forM_ allProjects $ \p -> do
        revDeps <- selectReverseDependencies p 
        deployableRevDeps <- getDeployableReverseDependencies p
        let result = if isImmediate then revDeps else deployableRevDeps

        when (not $ null result) $ do
            setColor Red
            T.IO.putStrLn p
            setColor Yellow
            mapM_ (T.IO.putStrLn . T.cons '\t') result
            putStrLn ""
    setColor White
  where 
    setColor c = setSGR [SetColor Foreground Dull c]

