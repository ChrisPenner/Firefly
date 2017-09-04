{-# language OverloadedStrings #-}
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
  , getCookies
  , getCookieMulti
  , getCookie
  ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI

import Web.Cookie
import qualified Network.Wai as W

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
getQuery key = M.lookup key <$> getQueries

getQueryMulti :: ReqReader m => T.Text -> m [T.Text]
getQueryMulti key = fromMaybe [] . M.lookup key <$> getQueriesMulti

getCookies :: ReqReader m => m (M.Map T.Text [T.Text])
getCookies = do
  headers <- getHeaders
  return $ case M.lookup (CI.mk "Cookie") headers of
             Just [cookieHeader] -> M.fromListWith mappend . fmap (second (:[])) . parseCookiesText . toBS $ cookieHeader
             _ -> M.empty

getCookieMulti :: ReqReader m => T.Text -> m [T.Text]
getCookieMulti key = fromMaybe [] . M.lookup key <$> getCookies

getCookie :: ReqReader m => T.Text -> m (Maybe T.Text)
getCookie key = listToMaybe <$> getCookieMulti key

isSecure :: ReqReader m => m Bool
isSecure = fromReq W.isSecure

getPathInfo :: ReqReader m => m [T.Text]
getPathInfo = fromReq W.pathInfo

waiRequest :: ReqReader m =>  m W.Request
waiRequest = asks request
