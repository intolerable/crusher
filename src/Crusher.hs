module Crusher where

import Args
import qualified Markup
import qualified Stylesheet

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Data.Text.Lazy (Text)
import Lucid (Html, renderText)
import Network.Wai.Parse (FileInfo(..))
import Stitch
import Stitch.Render
import System.Exit
import Web.Scotty.Trans
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import qualified System.Directory as System
import qualified System.IO as System
import qualified System.Process as Process

type ScottyM = ScottyT Text (ReaderT (Arguments Identity) IO)
type ActionM = ActionT Text (ReaderT (Arguments Identity) IO)
type HTML = Html ()

main :: IO ()
main = getArguments >>= go

go :: Arguments Identity -> IO ()
go args@(Arguments _ (Identity p) (Identity tmp)) =
  scottyT p (`runReaderT` args) (`runReaderT` args) $ do
      get "/" $ lucid Markup.homepage
      get "/stylesheet.css" $ stitch Stylesheet.style
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

lucid :: HTML -> ActionM ()
lucid = html . renderText

stitch :: CSS -> ActionM ()
stitch c = do
  printer <- lift (asks mode) >>= \case
    Production -> return compressed
    Development -> return basic
  setHeader "Content-Type" "text/css; charset=utf-8"
  text $ Text.fromChunks $ return $ renderCSSWith printer c

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

