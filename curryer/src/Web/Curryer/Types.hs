module Web.Curryer.Types where

import Control.Monad.Reader
import Control.Monad.Cont
import Network.Wai as W
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI

type App a = ReaderT ReqContext (ContT W.Response IO) a
type Handler a = App a

type Pattern = T.Text
type Route = T.Text
type HeaderMap = M.Map (CI.CI T.Text) [T.Text]
type MultiQueryMap = M.Map T.Text [T.Text]
type QueryMap = M.Map T.Text T.Text
type Body = T.Text
type ContentType = T.Text

data ReqContext = ReqContext
  { body :: T.Text
  , request :: W.Request
  , responder :: W.Response -> ContT W.Response IO ()
  }

newtype SerializationError =
  SerializationError T.Text
  deriving (Show)
