{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
module Web.Curryer.Response
  ( ToResponse(..)
  , ToBody(..)
  ) where

import Network.Wai as W
import qualified Data.Text as T
import Network.HTTP.Types.Status

import Web.Curryer.Internal.Utils

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

