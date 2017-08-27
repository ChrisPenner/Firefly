{-# language FlexibleContexts #-}
module Web.Curryer.Request
  ( path
  ) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Web.Curryer.App
import Web.Curryer.Internal.Utils

path :: MonadReader W.Request m => m T.Text
path = fromBS <$> asks W.rawPathInfo
