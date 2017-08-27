module Web.Curryer.Internal.Utils where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

toBS :: T.Text -> BS.ByteString
toBS = T.encodeUtf8

toLBS :: T.Text -> LBS.ByteString
toLBS = LT.encodeUtf8 . LT.fromStrict

fromBS :: BS.ByteString -> T.Text
fromBS = T.decodeUtf8
