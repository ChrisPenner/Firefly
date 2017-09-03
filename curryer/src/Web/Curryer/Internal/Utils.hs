module Web.Curryer.Internal.Utils where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI

import Web.Curryer.Types

import Network.HTTP.Types.Header
import qualified Data.Map as M

toBS :: T.Text -> BS.ByteString
toBS = T.encodeUtf8

toLBS :: T.Text -> LBS.ByteString
toLBS = LT.encodeUtf8 . LT.fromStrict

fromBS :: BS.ByteString -> T.Text
fromBS = T.decodeUtf8

fromLBS :: LBS.ByteString -> T.Text
fromLBS = LT.toStrict . LT.decodeUtf8

mkHeader :: T.Text -> T.Text -> Header
mkHeader headerName headerVal = (CI.mk (toBS headerName), toBS headerVal)

convertHeaders :: RequestHeaders -> HeaderMap
convertHeaders = M.unionsWith mappend . fmap embed
  where
    embed (name, val) = M.singleton (CI.map fromBS name) [fromBS val]
