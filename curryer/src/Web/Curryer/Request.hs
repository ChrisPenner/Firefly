{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
module Web.Curryer.Request
  ( getPath
  , getMethod
  , getQueryString
  , getQueries
  , getQueriesMulti
  , getQuery
  , getQueryMulti
  , getHeaders
  , getPathInfo
  , isSecure
  , getBody
  , waiRequest
  ) where

import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Wai as W
import qualified Data.Map as M

import Web.Curryer.Internal.Utils
import Web.Curryer.Types

type ReqReader m = MonadReader ReqContext m

fromReq :: ReqReader m => (W.Request -> a) -> m a
fromReq getter = asks (getter . request)

getPath, getMethod, getQueryString, getBody :: ReqReader m => m T.Text
getPath = fromBS <$> fromReq W.rawPathInfo
getMethod = fromBS <$> fromReq W.requestMethod
getQueryString = fromBS <$> fromReq W.rawQueryString
getBody = asks body


getHeaders :: ReqReader m => m HeaderMap
getHeaders = convertHeaders <$> fromReq W.requestHeaders

getQueriesMulti :: ReqReader m => m MultiQueryMap
getQueriesMulti = convertQueries <$> fromReq W.queryString

getQueries :: ReqReader m => m QueryMap
getQueries = simpleQuery <$>fromReq W.queryString

getQuery :: ReqReader m => T.Text -> m (Maybe T.Text)
getQuery query = M.lookup query <$> getQueries

getQueryMulti :: ReqReader m => T.Text -> m [T.Text]
getQueryMulti query = fromMaybe [] . M.lookup query <$> getQueriesMulti


isSecure :: ReqReader m => m Bool
isSecure = fromReq W.isSecure

getPathInfo :: ReqReader m => m [T.Text]
getPathInfo = fromReq W.pathInfo

waiRequest :: ReqReader m =>  m W.Request
waiRequest = asks request
