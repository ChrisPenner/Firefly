{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
module Web.Curryer (run) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import qualified Data.Map.Strict as M
import qualified Data.CaseInsensitive as CI

data Response = Response
  { _headers :: M.Map T.Text [T.Text]
  , _body :: T.Text
  , _status :: Status
  } deriving (Show, Eq)

baseResponse :: Response
baseResponse = 
  Response { _headers=M.empty
           , _body=""
           , _status=ok200
           }

type App = ReaderT W.Request (StateT Response IO) ()

run :: W.Port -> App -> IO ()
run p app = W.run p warpApp
  where
    warpApp req respond = do
      resp <- flip execStateT baseResponse . flip runReaderT req $ app
      respond $ toWaiResp resp

toBS :: T.Text -> BS.ByteString
toBS = T.encodeUtf8

toLBS :: T.Text -> LBS.ByteString
toLBS = LT.encodeUtf8 . LT.fromStrict

toWaiResp :: Response -> W.Response
toWaiResp Response{..} = W.responseLBS _status headerList (toLBS _body)
  where
    headerList = do
      (headerName, values) <- M.toList _headers
      val <- values
      return (CI.mk (toBS headerName), toBS val)
