module Crusher where

import Args
import qualified Markup

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Lucid (Html, renderText)
import Network.Wai.Parse (FileInfo(..))
import System.Exit
import Web.Scotty
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import qualified System.Directory as System
import qualified System.IO as System
import qualified System.Process as Process

main :: IO ()
main = getArguments >>= go

go :: Arguments Identity -> IO ()
go (Arguments _ (runIdentity -> p) (runIdentity -> tmp)) =
  scotty p $ do
    get "/" $ lucid Markup.homepage
    post "/upload" $ do
      fs <- Map.fromList <$> files
      case Map.lookup "upload" fs of
        Just (FileInfo _ _ content) -> do
          tempFilePath <- liftIO $ writeToTempFile tmp content
          liftIO (pngcrush tempFilePath) >>= \case
            Nothing -> do
              setHeader "Content-Type" "image/png"
              liftIO (ByteString.readFile tempFilePath) >>= raw
              liftIO $ System.removeFile tempFilePath
            Just err -> lucid $ Markup.error err
        Nothing -> lucid Markup.uploadNotFound

lucid :: Html a -> ActionM ()
lucid = html . renderText

writeToTempFile :: FilePath -> ByteString -> IO FilePath
writeToTempFile tmp content = do
  (fp, handle) <- System.openTempFile tmp "crush"
  ByteString.hPut handle content
  System.hClose handle
  return fp

pngcrush :: FilePath -> IO (Maybe String)
pngcrush fp = do
  (code, stdout, _) <- Process.readProcessWithExitCode "pngcrush" ["-brute", "-reduce", "-ow", fp] ""
  case code of
    ExitSuccess -> return Nothing
    ExitFailure _ -> return $ Just stdout

