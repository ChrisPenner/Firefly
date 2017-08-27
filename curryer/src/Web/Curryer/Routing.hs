{-# language OverloadedStrings #-}
module Web.Curryer.Routing
  ( route
  ) where

import Web.Curryer.Handler
import Web.Curryer.App
import Web.Curryer.Request
import Web.Curryer
import Network.HTTP.Types.Status
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Data.Text as T

route :: T.Text -> Handler (Status, T.Text) -> App (Status, T.Text) ()
route routePath handler = void . callCC $ \respond -> do
  pth <- path
  when (routePath == pth) $ do
    req <- ask
    response <- liftIO $ runReaderT handler req
    respond response
  return (notFound404, "")
