{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language TypeApplications #-}
module Web.Curryer
  ( run
  , route
  , module Network.HTTP.Types.Status
  , App
  , Handler
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.Cont

import qualified Data.Text as T

import Web.Curryer.Routing
import Web.Curryer.Types

run :: W.Port -> App () -> IO ()
run port app = W.run port warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req respond = runCurryer app req >>= respond

runCurryer :: App () -> W.Request -> IO W.Response
runCurryer app req = runContT (callCC unpackApp) return
  where
    appWith404 = app >> return notFoundResp
    unpackApp respond = runReaderT appWith404 (req, respond)

notFoundResp :: W.Response
notFoundResp = toResponse @(Status, T.Text) (notFound404, "Not Found")
