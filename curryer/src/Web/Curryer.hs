{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
module Web.Curryer
  ( run
  , App
  , Handler
  , ToResponse(..)
  , ToBody(..)
  , module Network.HTTP.Types.Status
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import qualified Data.Map.Strict as M
import qualified Data.CaseInsensitive as CI

import Web.Curryer.Handler
import Web.Curryer.App
import Web.Curryer.Internal.Utils

class ToResponse c where
  toResponse :: c -> W.Response

class ToBody b where
  toBody :: b -> T.Text
  contentType :: b -> T.Text

instance ToBody T.Text where
  toBody = id
  contentType _ = "text/plain"

instance (ToBody b) => ToResponse (Status, b) where
  toResponse (status, body)
    = W.responseLBS
          status
          [mkHeader "Content-Type" (contentType body)]
          (toLBS $ toBody body)


mkHeader :: T.Text -> T.Text -> Header
mkHeader headerName headerVal = (CI.mk (toBS headerName), toBS headerVal)

run :: W.Port -> App (Status, T.Text) () -> IO ()
run p app = W.run p warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req respond = do
      resp <- flip runContT (const $ return (notFound404, "Not Found")) . flip runReaderT req $ app :: IO (Status, T.Text)
      respond $ toResponse resp

