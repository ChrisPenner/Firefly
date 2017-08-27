{-# language OverloadedStrings #-}
module Main where

import Web.Curryer
import Web.Curryer.Routing
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: App (Status, T.Text) ()
app = route "/hello" hello

hello :: Handler (Status, T.Text)
hello = return (ok200, "Hello World!")
