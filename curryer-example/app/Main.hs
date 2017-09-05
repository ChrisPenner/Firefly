{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Main where

import Web.Curryer
import Data.Maybe
import Control.Monad.Trans
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Blaze.Html5 as H
import Data.Aeson
import GHC.Generics

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
  route "/greeter" greeter
  route "/users" getUser

hello :: App T.Text
hello = do
  name <- getQuery "name"
  return $ "Hello " `T.append` fromMaybe "Stranger" name

goodbye :: App T.Text
goodbye = do
  name <- getCookie "name"
  return $ "Goodbye " `T.append` fromMaybe "Stranger" name

greeter :: App H.Html
greeter = do
  name <- getQuery "name"
  return . greet $ fromMaybe "Stranger" name

data User = User
  { username::T.Text
  , age::Int
  } deriving (Generic, ToJSON, FromJSON)

steve :: User
steve = User{username="Steve", age=26}

getUser :: App (T.Text, Status)
getUser = do
  uname <- getQuery "username"
  when (uname == Just "steve") (respond $ Json steve)
  return ("Not found", notFound404)

greet :: T.Text -> H.Html
greet name = H.docTypeHtml $ do
  H.head $
    H.title "Hello!"
  H.body $ do
    H.h1 "Welcome to the Greeter!"
    H.p ("Hello " >> H.toHtml name >> "!")
