{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Main where

import Web.Curryer
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: App ()
app = do
  route "/hello" hello
  route "/goodbye" goodbye

hello :: App (Status, T.Text)
hello = return (ok200, "Hello World!")

goodbye :: App (Status, T.Text)
goodbye = return (ok200, "Goodbye World!")
