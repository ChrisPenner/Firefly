{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
module Web.Firefly.Request
  ( getPath
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
  ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI

import Web.Cookie
import qualified Network.Wai as W

import Web.Firefly.Internal.Utils
import Web.Firefly.Types

type ReqReader m = MonadReader ReqContext m

fromReq :: ReqReader m => (W.Request -> a) -> m a
fromReq getter = asks (getter . request)

-- | Calls through to 'W.rawPathInfo'; returns the full path of the current
-- request (without the query string)
getPath :: ReqReader m => m T.Text
getPath = fromBS <$> fromReq W.rawPathInfo

-- | Gets the HTTP method of the current request
getMethod :: ReqReader m => m T.Text
getMethod = fromBS <$> fromReq W.requestMethod

-- | Calls through to 'W.rawQueryString'; returns the full path of the current
-- request (without the query string)
getQueryString :: ReqReader m => m T.Text
getQueryString = fromBS <$> fromReq W.rawQueryString

-- | Gets full body of the request
getBody  :: ReqReader m => m T.Text
getBody = asks requestBody

-- | Gets the headers of the request
getHeaders :: ReqReader m => m HeaderMap
getHeaders = convertHeaders <$> fromReq W.requestHeaders

-- | Gets all key/values from the query string
getQueriesMulti :: ReqReader m => m MultiQueryMap
getQueriesMulti = convertQueries <$> fromReq W.queryString

-- | Get the last set value for each query
--
-- If a query is set without a value (e.g. "/?key") it will appear in the
-- map with a value of @""@
getQueries :: ReqReader m => m QueryMap
getQueries = simpleQuery <$>fromReq W.queryString

-- | Get the value for a given query. A query which was passed without a value
-- (e.g. "/?key") will return @Just ""@
getQuery :: ReqReader m => T.Text -> m (Maybe T.Text)
getQuery key = M.lookup key <$> getQueries

-- | Gets all values provided for a given query key
getQueryMulti :: ReqReader m => T.Text -> m [T.Text]
getQueryMulti key = fromMaybe [] . M.lookup key <$> getQueriesMulti

-- | Gets a map of cookies sent with the request.
getCookies :: ReqReader m => m (M.Map T.Text [T.Text])
getCookies = do
  headers <- getHeaders
  return $ case M.lookup (CI.mk "Cookie") headers of
             Just [cookieHeader] -> M.fromListWith mappend . fmap (second (:[])) . parseCookiesText . toBS $ cookieHeader
             _ -> M.empty

-- | Get all values set for a specific cookie
getCookieMulti :: ReqReader m => T.Text -> m [T.Text]
getCookieMulti key = fromMaybe [] . M.lookup key <$> getCookies

-- | Get the value for a cookie if it is set
getCookie :: ReqReader m => T.Text -> m (Maybe T.Text)
getCookie key = listToMaybe <$> getCookieMulti key

-- | Calls through to 'W.isSecure'
isSecure :: ReqReader m => m Bool
isSecure = fromReq W.isSecure

-- | Calls through to 'W.pathInfo'.
-- Returns the path's individual '/' separated chunks. '
getPathInfo :: ReqReader m => m [T.Text]
getPathInfo = fromReq W.pathInfo

-- | Exposes the underlying 'W.Request'.
waiRequest :: ReqReader m =>  m W.Request
waiRequest = asks request
