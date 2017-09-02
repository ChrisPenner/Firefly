{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
module Web.Curryer.Routing
  ( route
  , Respond
  , App
  , Handler
  ) where

-- import Web.Curryer.Handler
import Web.Curryer.App
import Web.Curryer.Request
-- import Web.Curryer
import Network.HTTP.Types.Status
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Data.Text as T

type App a = ContT W.Response (ReaderT W.Request IO) a
type Handler a = ReaderT W.Request IO a
type Respond = forall r. ToResponse r => r -> App ()

route :: ToResponse r => T.Text -> Handler r -> Respond -> App ()
route routePath handler respond = do
  liftIO $ print $ "Checking" `T.append` routePath
  pth <- path
  when (routePath == pth) $ do
    req <- ask
    response <- liftIO $ runReaderT handler req
    respond response
