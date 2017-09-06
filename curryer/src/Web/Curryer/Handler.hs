{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
module Web.Curryer.Handler
  ( route
  ) where

import Web.Curryer.Types
import Web.Curryer.Request
import Web.Curryer.Response
import Control.Monad.Cont
import Control.Monad.Reader
import qualified Data.Text as T

import Text.Regex.PCRE

-- | Run a handler on a matching route.
-- The handler may return any type which implements 'ToResponse'
--
-- If a route matches any actions following it in the App monad will not be run.
--
-- Route patterns support pcre regex but do not yet support url parameters.
--
-- In the following example if we call @helloHarry@ then the @helloHandler@
-- will run, but NOT the helloHarryHandler since a matching route
-- ceases further execution.
-- For this reason the order you define your routes in matters.
--
-- > app :: App ()
-- > app = do
-- >   route "/" indexHandler
-- >   route "/hello.*" helloHandler
-- >   route "/helloHarry" helloHarryHandler
route :: (ToResponse r) => Pattern -> Handler r -> App ()
route routePath handler = do
  path <- getPath
  when (routePath `matches` path) $ do
    response <- toResponse <$> handler
    resp <- asks responder
    lift $ resp response

-- | Determine whether a route matches a pattern
matches :: Route -> Pattern -> Bool
matches (T.unpack -> rt) (T.unpack -> pat) = ("^" ++ pat ++ "$") =~ rt
