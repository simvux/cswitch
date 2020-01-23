{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import System.Process
import qualified Data.Yaml          as Yaml
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.JSON.Generic

instance Yaml.FromJSON FullConfig

instance Yaml.ToJSON FullConfig

instance Yaml.FromJSON Config

instance Yaml.ToJSON Config

instance Yaml.FromJSON Dotfile

instance Yaml.ToJSON Dotfile

data FullConfig = FullConfig
  { configs          :: [Config]
  , run_after        :: [String]
  , source_container :: String
  } deriving (Generic, Show)

data Config = Config
  { identifier :: String
  , dotfiles   :: [Dotfile]
  } deriving (Generic, Show)

data Dotfile = Dotfile
  { source :: String
  , destin :: String
  } deriving (Generic, Show)

configPath = "/home/simon/.cswitch/cswitch.yml"

main :: IO ()
main = do
  configuration <- parseConfiguration
  mapM_
    (applyConfig $ stripSlash (source_container configuration))
    (configs configuration)
  mapM_ callCommand (run_after configuration)

applyConfig :: String -> Config -> IO ()
applyConfig container c = do
  toSwap <- get
  when (identifier c == toSwap) $ mapM_ (switch container) $ dotfiles c
  where
    get = last <$> getArgs

parseConfiguration :: IO FullConfig
parseConfiguration = do
  args <- getArgs
  let config_path = configPath
   in do configuration <-
           Yaml.decodeFileEither config_path :: IO (Either Yaml.ParseException FullConfig)
         case configuration of
           Left e  -> die $ Yaml.prettyPrintParseException e
           Right c -> return c

switch :: String -> Dotfile -> IO ()
switch container dotfile =
  copyFile (container ++ source dotfile) (destin dotfile)

stripSlash a =
  if last a == '/'
    then a
    else a ++ "/"
