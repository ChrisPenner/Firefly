{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Main where

import Web.Curryer
import Data.Maybe
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = run 3000 (loggerMiddleware app)

loggerMiddleware :: App () -> App ()
loggerMiddleware baseApp = do
  path <- getPath
  method <- getMethod
  liftIO . TIO.putStrLn $ "INFO: " `T.append` method `T.append` " to " `T.append` path
  baseApp

app :: App ()
app = do
  route "/hello" hello
  route "/goodbye" goodbye

hello :: App (Status, T.Text)
hello = do
  name <- getQuery "name"
  return (ok200, "Hello " `T.append` fromMaybe "Stranger" name)

goodbye :: App (Status, T.Text)
goodbye = do
  name <- getCookie "name"
  return (ok200, "Goodbye " `T.append` fromMaybe "Stranger" name)
