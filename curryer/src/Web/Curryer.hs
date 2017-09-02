{-# language RankNTypes #-}
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

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import qualified Data.Map.Strict as M

import Web.Curryer.Routing
import Web.Curryer.Handler
import Web.Curryer.App
import Web.Curryer.Internal.Utils

run :: W.Port -> (Respond -> App ()) -> IO ()
run p app = W.run p warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req respond = do
      resp <- runCurryer app req
      respond resp

runCurryer :: (Respond -> App ()) -> W.Request -> IO W.Response
runCurryer app req = flip runReaderT req $ flip runContT return $ callCC def
  where
    def resp = app (resp . toResponse) >> return (toResponse (notFound404, "Not Found" :: T.Text))
