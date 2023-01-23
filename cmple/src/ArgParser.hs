{-# OPTIONS_GHC -Wall #-}

module ArgParser (Args (..), parseArgs) where

import Config (LibraryType (..), ProjectType (..))
import Mode (Mode (..), NewProjectInfo (..))
import Options.Applicative

newtype Args = Args {mode :: Mode} deriving (Show)

parseArgs :: IO Args
parseArgs = execParser opts

opts :: ParserInfo Args
opts =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "cmple - The C minimal project manager"
        <> header "cmple - The C minimal project manager"
    )

argParser :: Parser Args
argParser = Args <$> parseMode

parseMode :: Parser Mode
parseMode =
  subparser
    ( new
        <> build
        <> run
        <> publish
    )

new :: Mod CommandFields Mode
new = command "new" $ info pProjectName (progDesc "create new project")

pProjectName :: Parser Mode
pProjectName = New <$> pNewProjectInfo

pNewProjectInfo :: Parser NewProjectInfo
pNewProjectInfo =
  NewProjectInfo
    <$> argument str (metavar "project name")
    <*> pProjectType

pProjectType :: Parser ProjectType
pProjectType =
  BinaryProject <$ flag' () (long "binary" <> help "binary project")
    <|> LibraryProject <$> pLibraryType

pLibraryType :: Parser LibraryType
pLibraryType =
  StaticLibrary <$ flag' () (long "libstatic" <> short 's' <> help "static library")
    <|> SharedLibrary <$ flag' () (long "libshared" <> short 'd' <> help "shared library")

build :: Mod CommandFields Mode
build = command "build" $ info (pure Build) (progDesc "build project")

run :: Mod CommandFields Mode
run = command "run" $ info (pure Run) (progDesc "run project")

publish :: Mod CommandFields Mode
publish = command "publish" $ info (pure Publish) (progDesc "publish project")