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

  -- * Utilities
  , addMiddleware

  -- * Exports
  -- | Re-exported types for your convenience
  , module Network.HTTP.Types.Status
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.Except

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
runCurryer app req = either id (const notFoundResp) <$> runExceptT unpackApp
  where
    unpackApp = do
      reqBody <- fmap fromLBS . liftIO $ W.strictRequestBody req
      runReaderT app ReqContext{request=req, requestBody=reqBody}

-- | Default 404 response
notFoundResp :: W.Response
notFoundResp = toResponse @(T.Text, Status) ("Not Found", notFound404)

-- | Run actions before your handlers and/or perform actions
-- following the response.
--
-- @after@ will only be run if a response is provided from some handler
addMiddleware :: App W.Request
              -- ^ The Action to run before a 'W.Request' is processed.
              -- The modified request which is returned will be passed to the app.
              -> (W.Response -> App W.Response)
              -- ^ Transform a 'W.Response' before it's sent
              -> App ()
              -- ^ The 'App' to wrap with middleware
              -> App ()
addMiddleware before after app = pre `catchError` post
  where
    post resp = after resp >>= throwError
    pre = before >>= \req -> local (\ctx -> ctx{request=req}) app
