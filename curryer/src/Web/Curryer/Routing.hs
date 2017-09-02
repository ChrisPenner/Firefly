{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Web.Curryer.Routing
  ( route
  ) where

import Web.Curryer.Types
import Web.Curryer.Request
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Data.Text as T


route :: ToResponse r => T.Text -> Handler r -> Respond -> App ()
route routePath handler respond = do
  liftIO $ print $ "Checking" `T.append` routePath
  pth <- path
  when (routePath == pth) $ do
    req <- ask
    response <- liftIO $ runReaderT handler req
    respond response
