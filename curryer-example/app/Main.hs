{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Main where

import Web.Curryer
import Web.Curryer.Routing
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: Respond -> App ()
app resp = do
  route "/hello" hello resp
  route "/goodbye" goodbye resp

hello :: Handler (Status, T.Text)
hello = return (ok200, "Hello World!")

goodbye :: Handler (Status, T.Text)
goodbye = return (ok200, "Goodbye World!")
