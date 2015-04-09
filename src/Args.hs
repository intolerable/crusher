module Args where

import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.Exit
import qualified System.Directory as System

type Port = Int

data Mode = Development | Production
  deriving (Show, Read, Eq)

modeToPort :: Mode -> Port
modeToPort Development = 3000
modeToPort Production = 80

data Arguments a =
  Arguments { mode :: Mode
            , port :: a Port
            , tempDir :: a FilePath }

deriving instance (Show (a Port), Show (a FilePath)) => Show (Arguments a)
deriving instance (Read (a Port), Read (a FilePath)) => Read (Arguments a)
deriving instance (Eq (a Port), Eq (a FilePath)) => Eq (Arguments a)

options :: Parser (Arguments Maybe)
options =
  Arguments <$> modeParser
            <*> optional portParser
            <*> optional tempDirParser

modeParser :: Parser Mode
modeParser = flag Development Production $ mconcat
  [ long "production"
  , help "Enable production mode" ]

portParser :: Parser Port
portParser = option auto $ mconcat
  [ long "port"
  , short 'p'
  , metavar "PORT"
  , help "Host the site on port PORT" ]

tempDirParser :: Parser FilePath
tempDirParser = option auto $ mconcat
  [ long "tmp-dir"
  , metavar "DIR"
  , help "Use DIR as scratch space for images" ]

validate :: Arguments Maybe -> IO (Arguments Identity)
validate (Arguments m p t) = do
  dir <- case t of
    Just x -> return x
    Nothing -> System.getTemporaryDirectory
  exists <- System.doesDirectoryExist dir
  if exists
    then return $ Arguments m p' (return dir)
    else do
      putStrLn "temp directory does not exist"
      exitFailure
  where
    p' = return $ fromMaybe (modeToPort m) p

getArguments :: IO (Arguments Identity)
getArguments = validate =<< execParser (info (helper <*> options) $ mconcat
  [ fullDesc
  , header "Site for crushing PNGs" ])
