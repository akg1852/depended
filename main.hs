{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Control.Monad
import System.IO
import Network.HTTP.Conduit
import System.Console.ANSI

import Data.Maybe
import Data.List
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO (putStrLn)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LazyChar8

import Model
import Parse


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
                        getDependencies name (fromJust packages) "package" (xGetAttribute "id")
                    when isNewProjHash $
                        getDependencies name (fromJust csproj) "ProjectReference" (xGetChild "Name")

                updateProjectDeployable name $ V.any (jNameIs "deploy.xml") files
  where
    isDir o = (jString . fromJust $ jLookup "type" o) == "dir"
    projFile name ext = T.concat [name, ".", ext, "proj"]
    jNameIs name o = (jString . fromJust $ jLookup "name" o) == name
    getDependencies projName file elName extractDepName = do
        xml <- simpleHttp . T.unpack . jString . fromJust $ jLookup "download_url" file
        mapM_ (insertRelationship projName) . fmap (fromJust . extractDepName) . xGetElements elName $ parseXML xml


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
    when ("--update" `elem` args) getData

    let search = case ("--search" `elemIndex` args) of
            Just i -> T.pack $ args !! succ i
            _ -> "%"
    let getDeps = if ("--immediate" `elem` args)
        then selectReverseDependencies
        else getDeployableReverseDependencies

    projects <- selectProjects search
    deps <- forM projects $ \p -> do { ds <- getDeps p; return (p, ds) }
    outputToCLI (filter (not . null . snd) deps) ("--plain" `notElem` args)


-- display

outputToCLI :: [(T.Text, [T.Text])] -> Bool -> IO ()
outputToCLI projects color = forM_ projects $ \(p, ds) -> do
    setColor Red
    T.IO.putStrLn p
    setColor Yellow
    mapM_ (T.IO.putStrLn . T.cons '\t') ds
    putStrLn ""
    setColor White
  where 
    setColor c = when color $ setSGR [SetColor Foreground Dull c]

