{-# language OverloadedStrings #-}
module Web.Curryer.Routing
  ( route
  , Respond
  , App
  , Handler
  ) where

-- import Web.Curryer.Handler
-- import Web.Curryer.App
import Web.Curryer.Request
-- import Web.Curryer
import Network.HTTP.Types.Status
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Data.Text as T

type App r a = ReaderT W.Request (ContT r IO) a
type Handler a = ReaderT W.Request IO a
type Respond = ((Status, T.Text) -> App (Status, T.Text) ())

route :: T.Text -> Handler (Status, T.Text) -> Respond -> App (Status, T.Text) ()
route routePath handler respond = do
  liftIO $ print $ "Checking" `T.append` routePath
  pth <- path
  liftIO $ do
      print pth
      print routePath
      print (pth == routePath)
  when (routePath == pth) $ do
    req <- ask
    response <- liftIO $ runReaderT handler req
    respond response
