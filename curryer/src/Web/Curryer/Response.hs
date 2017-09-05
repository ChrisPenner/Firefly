{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language UndecidableInstances #-}
module Web.Curryer.Response
  ( ToResponse(..)
  , Json(..)
  , respond
  ) where

import Data.Function ((&))
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson

import Network.Wai as W
import Network.HTTP.Types.Status

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Curryer.Internal.Utils
import Web.Curryer.Types

newtype Json a = Json a
  deriving Show

class ToResponse c where
  toResponse :: c -> W.Response

instance ToResponse T.Text where
  toResponse = mkResponse ok200 "text/plain"

instance ToResponse Html where
  toResponse = mkResponse ok200 "text/html" . TL.toStrict . renderHtml

instance ToResponse Aeson.Value where
  toResponse = mkResponse ok200 "application/json" . fromLBS . encode

instance (ToJSON a) => ToResponse (Json a) where
  toResponse (Json obj) = toResponse (toJSON obj)

instance ToResponse W.Response where
  toResponse = id

instance (ToResponse b) => ToResponse (b, Status) where
  toResponse (b, status) = toResponse (b, status, mempty :: HeaderMap)

instance (ToResponse b) => ToResponse (b, Status, HeaderMap) where
  toResponse (b, status, hm) =
        toResponse b
      & mapResponseStatus (const status)
      & mapResponseHeaders (++ fromHeaderMap hm)


mkResponse :: Status -> ContentType -> T.Text -> W.Response
mkResponse status contentType body =
  W.responseLBS status [mkHeader "Content-Type" contentType] (toLBS body)

respond :: ToResponse r => r -> App ()
respond r = do
  resp <- asks responder
  lift . resp $ toResponse r
