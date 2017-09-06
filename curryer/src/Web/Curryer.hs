{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language TypeApplications #-}
module Web.Curryer
  ( -- * Curryer Server
  run
  -- * Types
  , App
  , Handler

  -- * Handlers
  , route

  -- * Requests
  , getPath
  , getPathInfo
  , getMethod
  , getQueryString
  , getQueries
  , getQueriesMulti
  , getQuery
  , getQueryMulti
  , getHeaders
  , getBody
  , getCookies
  , getCookieMulti
  , getCookie
  , isSecure
  , waiRequest

  -- * Responses
  , ToResponse(..)
  , respond

  -- ** Wrapper Types
  , Json(..)

  -- * Exports
  -- | Re-exported types for your convenience
  , module Network.HTTP.Types.Status
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.Cont

import qualified Data.Text as T

import Web.Curryer.Handler
import Web.Curryer.Request
import Web.Curryer.Response
import Web.Curryer.Types
import Web.Curryer.Internal.Utils

-- | Run an http server on the given port.
--
-- > -- Run app on port 3000
-- > main :: IO ()
-- > main = run 3000 app
run :: W.Port -> App () -> IO ()
run port app = W.run port warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req resp = runCurryer app req >>= resp

-- | Run the app monad on a wai request to obtain a wai response
runCurryer :: App () -> W.Request -> IO W.Response
runCurryer app req = runContT (callCC unpackApp) return
  where
    appWith404 = app >> return notFoundResp
    unpackApp resp = do
      reqBody <- fmap fromLBS . liftIO $ W.strictRequestBody req
      runReaderT appWith404 ReqContext{request=req, responder=resp, requestBody=reqBody}

-- | Default 404 response
notFoundResp :: W.Response
notFoundResp = toResponse @(T.Text, Status) ("Not Found", notFound404)
