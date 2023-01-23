{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Config (Config (..), ProjectType (..), LibraryType (..), parseConfig, defaultConfig) where

import Data.Text (Text)
import TOML (DecodeTOML (..), Decoder, decodeFile, getField, invalidValue, makeDecoder)

data Config = Config
  { configUser :: String
  , configProjectName :: String
  , configProjectType :: ProjectType
  , configDependencies :: [String]
  }
  deriving (Show)

data ProjectType = BinaryProject | LibraryProject LibraryType deriving (Show)

data LibraryType = StaticLibrary | SharedLibrary deriving (Show)

instance DecodeTOML Config where
  tomlDecoder :: Decoder Config
  tomlDecoder =
    Config
      <$> getField "user"
      <*> getField "project"
      <*> getField "type"
      <*> getField "dependencies"

instance DecodeTOML ProjectType where
  tomlDecoder :: Decoder ProjectType
  tomlDecoder = toProjectType =<< tomlDecoder
   where
    toProjectType :: Text -> Decoder ProjectType
    toProjectType = \case
      "binary" -> pure BinaryProject
      "library static" -> pure $ LibraryProject StaticLibrary
      "library shared" -> pure $ LibraryProject SharedLibrary
      _ -> makeDecoder $ invalidValue "Invalid project type"

parseConfig :: IO Config
parseConfig = do
  result <- decodeFile "build.toml"
  case result of
    Right cfg -> return cfg
    Left e -> error $ show e

defaultConfig :: String -> ProjectType -> (String, Config)
defaultConfig projectName projectType =
  ( ( "user = "
        ++ defaultUser
        ++ "\n"
    )
      ++ ( "project = \""
            ++ projectName
            ++ "\"\n"
         )
      ++ ( "type = "
            ++ projType
            ++ "\n"
         )
      ++ ( "dependencies = "
            ++ show defaultDependencies
            ++ "\n"
         )
  , Config
      { configUser = defaultUser
      , configProjectType = projectType
      , configProjectName = projectName
      , configDependencies = defaultDependencies
      }
  )
 where
  defaultUser = "\"\""
  defaultDependencies = [] :: [String]
  projType = case projectType of
    BinaryProject -> "\"binary\""
    LibraryProject StaticLibrary -> "\"library static\""
    LibraryProject SharedLibrary -> "\"library shared\""
