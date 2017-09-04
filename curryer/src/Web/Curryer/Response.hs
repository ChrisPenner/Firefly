{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
module Web.Curryer.Response
  ( ToResponse(..)
  , ToBody(..)
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson

import Network.Wai as W
import Network.HTTP.Types.Status

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Curryer.Internal.Utils
import Web.Curryer.Types

class ToResponse c where
  toResponse :: c -> W.Response

class ToBody b where
  toBody :: b -> Either SerializationError (Body, ContentType)

instance ToBody T.Text where
  toBody txt = return (txt, "text/plain")

instance ToBody Html where
  toBody txt = return (TL.toStrict (renderHtml txt), "text/html")

instance ToBody Aeson.Value where
  toBody b = case fromJSON b of
               Error err -> Left $ SerializationError (T.pack err)
               Success txt -> Right (txt, "application/json")

newtype Json a = Json a
  deriving Show

instance (ToJSON a) => ToBody (Json a) where
  toBody (Json obj) = Right (fromLBS $ encode obj, "application/json")

instance ToResponse W.Response where
  toResponse = id

instance (ToBody b) => ToResponse (Status, b) where
  toResponse (status, respBody) =
    case toBody respBody of
      Left (SerializationError err) -> W.responseLBS internalServerError500 [mkHeader "Content-Type" "text/plain"] (toLBS err)
      Right (b, ct) -> W.responseLBS status [mkHeader "Content-Type" ct] (toLBS b)
