{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Mode (Mode (..), NewProjectInfo (..), execMode) where

import Codec.Archive.Zip (CompressionMethod (Deflate), createArchive, loadEntry, mkEntrySelector, packDirRecur, unpackInto, withArchive)
import Config (Config (..), LibraryType (..), ProjectType (..), defaultConfig, parseConfig)
import Control.Conditional (ifM, unlessM, whenM)
import Control.DeepSeq (($!!))
import Control.Lens ((^.))
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Text (pack)
import GHC.IO.Exception (ExitCode (..))
import Network.Wreq (FormParam ((:=)), partFile, partText, post, responseBody)
import System.Directory (createDirectory, createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, doesFileExist, removeFile, withCurrentDirectory)
import System.Directory.Recursive (getFilesRecursive)
import System.Process (system)

data Mode
  = New NewProjectInfo
  | Build
  | Run
  | Publish
  deriving (Show)

data State = State
  { stateConfig :: Config
  , stateSourceFiles :: [String]
  , stateIncludeFiles :: [String]
  }
  deriving (Show)

makeState :: Config -> IO State
makeState cfg@Config{configDependencies = deps} = do
  srcFiles <- getSourceFiles
  incFiles <- getIncludeFiles
  unless (null deps) $ getDependencies cfg
  return State{stateConfig = cfg, stateSourceFiles = srcFiles, stateIncludeFiles = incFiles}

data NewProjectInfo = NewProjectInfo
  { projectInfoName :: String
  , projectInfoType :: ProjectType
  }
  deriving (Show)

execMode :: Mode -> IO ()
execMode (New NewProjectInfo{projectInfoName = projectName, projectInfoType = projectType}) = do
  createDirectory projectName
  let (str, config) = defaultConfig projectName projectType
  writeFile (projectName ++ "/build.toml") str
  createDirectory $ projectName ++ "/source"
  createDirectoryIfMissing True $ projectName ++ "/header/private"
  createDirectoryIfMissing True $ projectName ++ "/header/public"
  withCurrentDirectory (configProjectName config) $ do
    state <- makeState config
    updateCMakeLists state
execMode Build = do
  config <- parseConfig
  state <- makeState config
  updateCMakeLists state
  build
execMode Run = do
  config <- parseConfig
  state <- makeState config
  updateCMakeLists state
  run state
execMode Publish = do
  config <- parseConfig
  state <- makeState config
  updateCMakeLists state
  publish state

run :: State -> IO ()
run State{stateConfig = Config{configProjectType = LibraryProject _}} = error "Can only run binary projects"
run State{stateConfig = Config{configProjectName = projectName}} = do
  build
  putStrLn "Running..."
  system ("target/" ++ projectName) >>= \case
    ExitSuccess -> return ()
    ExitFailure c -> error $ "Run failed with exit code " ++ show c
  return ()

publish :: State -> IO ()
publish State{stateConfig = Config{configProjectType = BinaryProject}} = error "Can only publish library projects"
publish State{stateConfig = Config{configUser = user, configProjectName = projectName}} = do
  build
  putStrLn "Publishing..."
  compress
  upload
 where
  compress = do
    putStrLn "- Zipping..."
    whenM (doesFileExist zipName) (removeFile zipName)

    createArchive zipName $ do
      buildEntrySelector <- mkEntrySelector "build.toml"
      loadEntry Deflate buildEntrySelector "build.toml"

      cmlEntrySelector <- mkEntrySelector "CMakeLists.txt"
      loadEntry Deflate cmlEntrySelector "CMakeLists.txt"

      packDirRecur Deflate (mkEntrySelector . ("source/" ++)) "source"
      packDirRecur Deflate (mkEntrySelector . ("header/" ++)) "header"

  upload = do
    putStrLn "- Uploading..."
    _ <-
      post
        "https://6mybbf.deta.dev/uploadfiles"
        [ partFile "file" zipName
        , partText "user" (pack user)
        , partText "project" (pack projectName)
        ]
    return ()
  zipName = projectName ++ ".zip"

reload :: IO ()
reload = do
  putStrLn "Reloading..."
  unlessM (doesDirectoryExist "target") (createDirectory "target")
  system "cmake -B target -S ." >>= \case
    ExitSuccess -> return ()
    ExitFailure c -> error $ "Reload failed with exit code " ++ show c
  return ()

build :: IO ()
build = do
  reload
  putStrLn "Building..."
  system "cmake --build target" >>= \case
    ExitSuccess -> return ()
    ExitFailure c -> error $ "Build failed with exit code " ++ show c
  return ()

getDependencies :: Config -> IO ()
getDependencies Config{configDependencies = deps} = do
  createDirectoryIfMissing True "depends"
  createDirectoryIfMissing True "lib"
  mapM_ go deps
 where
  go proj = do
    let projDir = "depends/" ++ proj ++ "/"
    let zipPath = "depends/" ++ proj ++ ".zip"
    putStrLn $ "- Processing dependency " ++ proj
    ifM (doesDirectoryExist projDir) (putStrLn "- Already downloaded, skipping...") $ do
      r <- post "https://6mybbf.deta.dev/fetch" ["project" := proj]
      createDirectory projDir
      LBS.writeFile zipPath (r ^. responseBody)
      putStrLn "- Unzipping..."
      withArchive zipPath (unpackInto projDir)
      putStrLn "- Removing zip file..."
      removeFile zipPath
      buildDependency proj
      putStrLn "- Symlinking public headers..."
      createDirectoryLink ("../" ++ projDir ++ "header/public") ("lib/" ++ proj)
      return ()

buildDependency :: String -> IO ()
buildDependency dep = do
  putStrLn "- Building dependency..."
  withCurrentDirectory ("depends/" ++ dep) (execMode Build)

getSourceFiles :: IO [String]
getSourceFiles = do
  files <- getFilesRecursive "source"
  return $ filter (".c" `isSuffixOf`) files

getIncludeFiles :: IO [String]
getIncludeFiles = do
  files <- getFilesRecursive "header"
  return $ filter (".h" `isSuffixOf`) files

updateCMakeLists :: State -> IO ()
updateCMakeLists state = do
  ifM
    (doesFileExist "CMakeLists.txt")
    ( do
        previous <- readFile "CMakeLists.txt"
        let next = makeCMakeLists (Just previous) state
        writeFile "CMakeLists.txt" $!! next
    )
    (writeFile "CMakeLists.txt" (makeCMakeLists Nothing state))

makeCMakeLists :: Maybe String -> State -> String
makeCMakeLists
  previous
  State
    { stateConfig =
      Config
        { configProjectName = projectName
        , configProjectType = projectType
        , configDependencies = deps
        }
    , stateSourceFiles = srcFiles
    } =
    "#[[ DIRE THINGS WILL HAPPEN TO WHOM DARES TO EDIT THE LINES BETWEEN THESE COMMENTS\n"
      ++ "    Add any extra CMake commands under the matching comment below ]]\n"
      ++ "cmake_minimum_required(VERSION 3.25)\n"
      ++ "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)\n"
      ++ ( "project("
            ++ projectName
            ++ ")\n"
         )
      ++ target
      ++ ( "target_include_directories("
            ++ projectName
            ++ " PUBLIC header"
            ++ ( if null deps
                  then ""
                  else " lib"
               )
            ++ ")\n"
         )
      ++ ( if null deps
            then "\n"
            else
              "target_link_directories("
                ++ projectName
                ++ " PUBLIC "
                ++ unwords (map (\d -> "\"depends/" ++ d ++ "/target\"") deps)
                ++ ")\n"
         )
      ++ ( if null deps
            then "\n"
            else
              "target_link_libraries("
                ++ projectName
                ++ " PUBLIC "
                ++ unwords (map (\d -> "\"" ++ d ++ "\"") deps)
                ++ ")\n"
         )
      ++ "#[[ Add any extra CMake commands below this line ]]\n"
      ++ maybe
        ""
        (concatMap (\l -> if l == "" then "\n" else l) . drop 10 . lines)
        previous
   where
    target = case projectType of
      BinaryProject ->
        "add_executable("
          ++ projectName
          ++ " "
          ++ unwords srcFiles
          ++ ")\n"
      LibraryProject t -> case t of
        StaticLibrary ->
          "add_library("
            ++ projectName
            ++ " STATIC "
            ++ unwords srcFiles
            ++ ")\n"
        SharedLibrary ->
          "add_library("
            ++ projectName
            ++ " SHARED "
            ++ unwords srcFiles
            ++ ")\n"