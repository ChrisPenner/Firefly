{-# language OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Main where

import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Blaze.Html5 as H
import qualified Network.Wai as W

import Web.Firefly

-- | The Main app; this runs our app on port 3000 with logger middleware
main :: IO ()
main = run 3000 (loggerMiddleware app)

app :: App ()
app = do
  route "/hello" helloHandler
  route "/goodbye/.*" goodbyeHandler
  route "/users" getUser

-- | Get the 'name' query param from the url, if it doesn't exist use
-- 'Stranger'
helloHandler :: Handler T.Text
helloHandler = do
  name <- fromMaybe "Stranger" <$> getQuery "name"
  -- If we just return some Text the response will be status 200 with
  -- a Content-Type of "text/plain"
  return $ "Hello " <> name

goodbyeHandler :: Handler H.Html
goodbyeHandler = do
  pathInfo <- getPathInfo
  return . byeHtml $
    case pathInfo of
      [_, name] -> if name == "" then "Stranger"
                                 else name
      _ -> "Stranger"

byeHtml :: T.Text -> H.Html
byeHtml name = H.docTypeHtml $ do
  H.head $
    H.title "Goodbye!"
  H.body $ do
    H.h1 "Goodbye!"
    H.p ("Bye " >> H.b (H.toHtml name) >> " thanks for coming!")

data User = User
  { username::T.Text
  , age::Int
  } deriving (Generic, ToJSON, FromJSON) -- Derive JSON instances

-- A reguler 'ol user
steve :: User
steve = User{username="Steve", age=26}

-- | Get a user by username
getUser :: Handler W.Response
getUser = do
  uname <- getQuery "username"
  return $ case uname of
             -- | We can use 'toResponse' to convert differing response types
             -- to a common Wai Response.
             Just "steve" -> toResponse $ Json steve -- If you just provide a body the status defaults to 200
             Just name -> toResponse ("Couldn't find user: " <> name, notFound404)
             Nothing -> toResponse ("Please provide a 'username' parameter" :: T.Text, badRequest400)

-- | We can add middleware before and after processing a request.
-- We'll just log some info without editing the request/response
loggerMiddleware :: App () -> App ()
loggerMiddleware = addMiddleware before after
  where
    before = do
      path <- getPath;
      method <- getMethod;
      liftIO . TIO.putStrLn $ "INFO: " <> method <> " to " <> path
      waiRequest -- Return the request we want to run the app with. In this case we didn't make changes

    after resp = do
      liftIO $ print (W.responseStatus resp)
      return resp -- Return the response with any changes, we didn't make any changes
