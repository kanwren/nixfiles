{-# language ImportQualifiedPost #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main where

import Control.Exception
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Traversable (for)
import Lucid hiding (for_)
import Network.HTTP.Types qualified as HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Options.Applicative
import System.Directory qualified as Dir
import System.FilePath qualified as Path
import System.IO.Error (isPermissionError)

htmlResponse :: HTTP.Types.Status -> Html () -> Wai.Response
htmlResponse status = Wai.responseLBS status [("Content-Type","text/html")] . renderBS

mkApp :: [Wai.Middleware] -> Wai.Application
mkApp = foldr ($) (\_ respond -> respond response404)
  where response404 = htmlResponse HTTP.Types.status404 $ h1_ "404: File Not Found"

listing :: FilePath -> Wai.Middleware
listing dir app req respond = maybe (app req respond) makeListing =<< candidatePath
  where
    makeListing :: FilePath -> IO Wai.ResponseReceived
    makeListing fp = handle reportErrors $ do
      entries <- map (fp Path.</>) . List.sort <$> Dir.listDirectory fp
      renderedEntries <- for entries $ \entry -> do
        suffix <- do
          let when' b x = if b then x else mempty
          isDir <- Dir.doesDirectoryExist entry
          isSymlink <- Dir.pathIsSymbolicLink entry
          pure $ when' isDir "/" <> when' isSymlink "@"
        pure ('/' : Path.makeRelative dir entry, Path.makeRelative fp entry <> suffix)
      respond $ htmlResponse HTTP.Types.status200 $ doctypehtml_ $ do
        head_ $ do
          meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "utf-8"]
          title_ $ toHtml fp
        body_ $ do
          h1_ $ toHtml $ "Directory listing for " <> fp
          hr_ []
          ul_ $ for_ renderedEntries $ \(url, name) -> do
            li_ $ a_ [href_ (Text.pack url)] $ toHtml name
          hr_ []
      where
        reportErrors e = do
          if isPermissionError e
          then respond $ htmlResponse HTTP.Types.status403 $ do
            h1_ "403: Forbidden"
            p_ "No permission to list directory"
          else respond $ htmlResponse HTTP.Types.status500 $ do
            h1_ "500: Internal Server Error"
            p_ $ toHtml $ show e

    candidatePath :: IO (Maybe FilePath)
    candidatePath = do
      let method = Wai.requestMethod req
      let pathParts = Wai.pathInfo req
      let path = Text.unpack $ Text.intercalate "/" pathParts
      if (method == HTTP.Types.methodHead || method == HTTP.Types.methodGet)
        && not (Path.isAbsolute path) && ".." `notElem` pathParts
      then do
        exists <- Dir.doesDirectoryExist $ dir Path.</> path
        pure $ if exists then Just (dir Path.</> path) else Nothing
      else pure Nothing

data Args = Args { _argsDir :: Maybe FilePath, _argsPort :: Int }

args :: Parser Args
args = Args
  <$> do argument (Just <$> str) $ metavar "DIR" <> help "Directory to serve (Default: current directory)" <> value Nothing
  <*> do option auto $ short 'p' <> long "port" <> metavar "PORT" <> help "Port to serve over (Default: 8080)" <> value 8080

parseArgs :: IO Args
parseArgs = customExecParser parserPrefs $ info (args <**> helper) mempty
  where parserPrefs = prefs showHelpOnEmpty

main :: IO ()
main = do
  Args{_argsPort = port, _argsDir = dir} <- parseArgs
  curDir <- Dir.getCurrentDirectory
  Dir.setCurrentDirectory $ maybe curDir (curDir Path.</>) dir
  dir' <- Dir.getCurrentDirectory
  putStrLn $ "Serving " <> dir' <> " on port " <> show port <> "..."
  Warp.run port $ mkApp [Wai.logStdout, listing dir', Wai.static]

