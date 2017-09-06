{-# language OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module Main where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Network.Wai as W
import Web.Curryer

-- | The Main app; this runs our app on port 3000 with logger middleware
main :: IO ()
main = run 3000 app

app :: App ()
app = do
  route "/users" getUser

data User = User
  { username::T.Text
  , age::Int
  } deriving (Generic, ToJSON, FromJSON) -- Derive JSON instances

-- A reguler 'ol user
steve :: User
steve = User{username="Steve", age=26}

-- | Get a user by username
getUser :: App W.Response
getUser = do
  uname <- getQuery "username"
  return $ case uname of
             -- | We can use 'toResponse' to convert differing response types
             -- to a common Wai Response.
             Just "steve" -> toResponse $ Json steve -- If you just provide a body the status defaults to 200
             Just name -> toResponse ("Couldn't find user: " `mappend` name, notFound404)
             Nothing -> toResponse ("Please provide a 'username' parameter" :: T.Text, badRequest400)
