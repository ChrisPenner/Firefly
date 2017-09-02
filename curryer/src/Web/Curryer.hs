{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language TypeApplications #-}
module Web.Curryer
  ( run
  , module Network.HTTP.Types.Status
  , App
  , Handler
  , Respond
  ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import Network.HTTP.Types.Status

import Control.Monad.Reader
import Control.Monad.Cont

import qualified Data.Text as T

import Web.Curryer.Routing
import Web.Curryer.Types

run :: W.Port -> (Respond -> App ()) -> IO ()
run p app = W.run p warpApp
  where
    warpApp :: W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
    warpApp req respond = do
      resp <- runCurryer app req
      respond resp

runCurryer :: (Respond -> App ()) -> W.Request -> IO W.Response
runCurryer app req = flip runReaderT req $ flip runContT return $ callCC def
  where
    def resp = app (resp . toResponse) >> return (toResponse @(Status, T.Text) (notFound404, "Not Found"))
