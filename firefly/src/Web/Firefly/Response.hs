{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language UndecidableInstances #-}
module Web.Firefly.Response
  ( ToResponse(..)
  , Json(..)
  , respond
  ) where

import Data.Function ((&))
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson

import Network.Wai as W
import Network.HTTP.Types.Status

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Firefly.Internal.Utils
import Web.Firefly.Types

-- | A simple newtype wrapper you can use to wrap values, signifying
-- they should be JSON encoded sent with the "application/json"
-- Content-Type.
newtype Json a = Json a
  deriving Show

-- | This class represents all types which can be converted into a valid
-- 'W.Response'. Feel free to implement additional instances for your own
-- data-types.
class ToResponse c where
  toResponse :: c -> W.Response

instance ToResponse String where
  toResponse = mkResponse ok200 "text/plain" . T.pack

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

-- | Respond to the client immediately. Any statements following this one
-- in the App or Handler Monads will not be run.
respond :: ToResponse r => r -> App ()
respond =  throwError . toResponse
