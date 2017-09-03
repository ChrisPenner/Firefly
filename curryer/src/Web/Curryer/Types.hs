{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language ConstraintKinds #-}
module Web.Curryer.Types
  ( ToResponse(..)
  , ToBody(..)
  , App
  , Handler
  , Pattern
  , Route
  ) where

import Control.Monad.Reader
import Control.Monad.Cont
import Network.Wai as W
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import Web.Curryer.Internal.Utils

type App a = ReaderT (W.Request, W.Response -> ContT W.Response IO ()) (ContT W.Response IO) a
type Handler a = App a

type Pattern = T.Text
type Route = T.Text

class ToResponse c where
  toResponse :: c -> W.Response

class ToBody b where
  toBody :: b -> T.Text
  contentType :: b -> T.Text

instance ToBody T.Text where
  toBody = id
  contentType _ = "text/plain"

instance ToResponse W.Response where
  toResponse = id

instance (ToBody b) => ToResponse (Status, b) where
  toResponse (status, body)
    = W.responseLBS
          status
          [mkHeader "Content-Type" (contentType body)]
          (toLBS $ toBody body)

mkHeader :: T.Text -> T.Text -> Header
mkHeader headerName headerVal = (CI.mk (toBS headerName), toBS headerVal)

