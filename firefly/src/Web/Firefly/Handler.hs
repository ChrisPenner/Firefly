{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
module Web.Firefly.Handler
  ( route
  ) where

import Web.Firefly.Types
import Web.Firefly.Request
import Web.Firefly.Response
import Control.Monad

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
  doesPathMatch <- pathMatches routePath
  when doesPathMatch (handler >>= respond . toResponse)
