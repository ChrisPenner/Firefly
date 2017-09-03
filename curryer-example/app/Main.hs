{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Main where

import Web.Curryer
import Data.Maybe
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: App ()
app = do
  route "/hello" hello
  route "/goodbye" goodbye

hello :: App (Status, T.Text)
hello = do
  name <- getQuery "name"
  return (ok200, "Hello " `T.append` fromMaybe "Stranger" name)

goodbye :: App (Status, T.Text)
goodbye = return (ok200, "Goodbye World!")
