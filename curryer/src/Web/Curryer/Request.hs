{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
module Web.Curryer.Request
  ( getPath
  , getMethod
  , getQueryString
  , getQueryParams
  , getHeaders
  , getPathInfo
  , isSecure
  , getBody
  ) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Network.Wai as W

import Web.Curryer.Internal.Utils
import Web.Curryer.Types

type ReqReader m = MonadReader ReqContext m

fromReq :: ReqReader m => (W.Request -> a) -> m a
fromReq getter = asks (getter . request)

getPath, getMethod, getQueryString :: ReqReader m => m T.Text
getPath = fromBS <$> fromReq W.rawPathInfo
getMethod = fromBS <$> fromReq W.requestMethod
getQueryString = fromBS <$> fromReq W.rawQueryString

getHeaders :: ReqReader m => m HeaderMap
getHeaders = convertHeaders <$> fromReq W.requestHeaders

getQueryParams :: ReqReader m => m QueryMap
getQueryParams = convertQueries <$> fromReq W.queryString

isSecure :: ReqReader m => m Bool
isSecure = fromReq W.isSecure

getPathInfo :: ReqReader m => m [T.Text]
getPathInfo = fromReq W.pathInfo

getBody :: ReqReader m =>  m T.Text
getBody = asks body
