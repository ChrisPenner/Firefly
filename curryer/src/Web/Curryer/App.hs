module Web.Curryer.App
  (--  App
 --  , Handler
  ) where

import Control.Monad.Reader
import Control.Monad.Cont
import Network.Wai as W

type App r a = ReaderT W.Request (ContT r IO) a
type Handler a = ReaderT W.Request IO a
