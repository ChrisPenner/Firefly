{-# language OverloadedStrings #-}
module Main where

import Web.Curryer
import qualified Data.Text as T

main :: IO ()
main = run 3000 hello

hello :: App (Status, T.Text)
hello = return (ok200, "Hello World!")
