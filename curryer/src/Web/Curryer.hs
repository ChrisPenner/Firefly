{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language TypeApplications #-}
module Web.Curryer
  ( run
  , route
  , App
  , Handler
  , ToResponse(..)
  , Json(..)
  , respond
  , module Network.HTTP.Types.Status
  , module Web.Curryer.Request
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.Cont

import qualified Data.Text as T

import Web.Curryer.Request
import Web.Curryer.Routing
import Web.Curryer.Response
import Web.Curryer.Types
import Web.Curryer.Internal.Utils

run :: W.Port -> App () -> IO ()
run = runWith return

runWith :: (W.Response -> IO W.Response) -> W.Port -> App () -> IO ()
runWith after port app = W.run port warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req resp = runCurryer app req >>= after >>= resp

runCurryer :: App () -> W.Request -> IO W.Response
runCurryer app req = runContT (callCC unpackApp) return
  where
    appWith404 = app >> return notFoundResp
    unpackApp resp = do
      reqBody <- fmap fromLBS . liftIO $ W.strictRequestBody req
      runReaderT appWith404 ReqContext{request=req, responder=resp, requestBody=reqBody}

notFoundResp :: W.Response
notFoundResp = toResponse @(T.Text, Status) ("Not Found", notFound404)
