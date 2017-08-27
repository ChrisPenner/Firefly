{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
module Web.Curryer
  ( run
  , App
  , module Network.HTTP.Types.Status
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

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

class ToResponse c where
  toResponse :: c -> W.Response

class ToBody b where
  toBody :: b -> LBS.ByteString
  contentType :: b -> T.Text

instance ToBody T.Text where
  toBody = toLBS
  contentType _ = "text/plain"

instance (ToBody b) => ToResponse (Status, b) where
  toResponse (status, body)
    = W.responseLBS status [mkHeader "Content-Type" (contentType body)] (toBody body)

type App c = ReaderT W.Request IO c

mkHeader :: T.Text -> T.Text -> Header
mkHeader headerName headerVal = (CI.mk (toBS headerName), toBS headerVal)

run :: ToResponse r => W.Port -> App r -> IO ()
run p app = W.run p warpApp
  where
    warpApp req respond = do
      resp <- runReaderT app req
      respond $ toResponse resp

toBS :: T.Text -> BS.ByteString
toBS = T.encodeUtf8

toLBS :: T.Text -> LBS.ByteString
toLBS = LT.encodeUtf8 . LT.fromStrict
