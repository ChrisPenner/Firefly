{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
module Web.Curryer.Routing
  ( route
  ) where

import Web.Curryer.Types
import Web.Curryer.Request
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Data.Text as T

import Text.Regex.PCRE

route :: (ToResponse r) => T.Text -> App r -> App ()
route routePath handler = do
  path <- getPath
  when (routePath `matches` path) $ do
    response <- toResponse <$> handler
    respond <- asks snd
    lift $ respond response

matches :: Route -> Pattern -> Bool
matches (T.unpack -> rt) (T.unpack -> pat) = ("^" ++ pat ++ "$") =~ rt
