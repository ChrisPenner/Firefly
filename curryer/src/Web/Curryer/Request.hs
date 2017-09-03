{-# language FlexibleContexts #-}
module Web.Curryer.Request
  ( getPath
  ) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Network.Wai as W

import Web.Curryer.Internal.Utils

getPath :: MonadReader (W.Request, q) m => m T.Text
getPath = fromBS <$> asks (W.rawPathInfo . fst)
