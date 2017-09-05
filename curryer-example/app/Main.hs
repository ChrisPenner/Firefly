{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TypeApplications #-}
module Main where

import Web.Curryer
import Data.Maybe
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Blaze.Html5 as H
import Data.Aeson
import GHC.Generics
import Network.Wai as W

main :: IO ()
main = run 3000 (loggerMiddleware app)

loggerMiddleware :: App () -> App ()
loggerMiddleware baseApp = do
  path <- getPath
  method <- getMethod
  liftIO . TIO.putStrLn $ "INFO: " <> method <> " to " <> path
  baseApp

app :: App ()
app = do
  route "/hello" hello
  route "/goodbye" goodbye
  route "/greeter" greeter
  route "/users/." getUser

hello :: App T.Text
hello = do
  name <- getQuery "name"
  return $ "Hello " <> fromMaybe "Stranger" name

goodbye :: App T.Text
goodbye = do
  name <- getCookie "name"
  return $ "Goodbye " <> fromMaybe "Stranger" name

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

getUser :: App W.Response
getUser = do
  uname <- getQuery "username"
  return $ case uname of
             Just "steve" -> toResponse $ Json steve
             Just name -> toResponse $ "Couldn't find user: " <> name
             Nothing -> toResponse @T.Text "Please provide a 'username' parameter"

greet :: T.Text -> H.Html
greet name = H.docTypeHtml $ do
  H.head $
    H.title "Hello!"
  H.body $ do
    H.h1 "Welcome to the Greeter!"
    H.p ("Hello " >> H.toHtml name >> "!")
