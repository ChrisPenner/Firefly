# Firefly

- [Example App](./firefly-example/app/Main.hs)
- [Hackage Docs](http://hackage.haskell.org/package/firefly-0.1.0.0/docs/Web-Firefly.html)

Firefly is dead simple http framework written in Haskell.

It strives for simplicity in implementation (and in use).
It's great for people learning Haskell, fiddling with Monads,
or who just need a really simple server for something.

Here's the minimal app:

```haskell
{-# language OverloadedStrings #-}
import Web.Firefly
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: App ()
app = do
  route "/hello" (return "hello" :: Handler T.Text)
```

Just that easy!

Check out the [Example App](./firefly-example/app/Main.hs) for more!

Specify your routes using regex patterns, the first one which matches will run.

`Handler` is a monad with access to the incoming request. You can access parts
of it using helpers, then return a response.

Here are some valid response types and their inferred Content-Type

- `Data.Text.Text`: `text/plain` 
- `Data.Aeson.Value`: `application/json` 
- `Blaze.Html.Html`: `text/html`

There are more in `Web.Firefly.Response`.

You can specify your status code using `(body, Status)` where body is any of
the above types and Status is an Integer status code.

Or, add headers too with `(body, Status, HeaderMap)` where `HeaderMap` is a map
of names to values.

## Examples

Let's write some more interesting handlers:

```haskell
hello :: App T.Text
hello = do
  -- | Get the 'name' query param from the url, if it doesn't exist use 'Stranger'
  name <- getQuery "name"
  -- If we just return some Text the response will be status 200 with a Content-Type of "text/plain"
  return $ "Hello " <> fromMaybe "Stranger" name
```

Here's an example of responding with JSON:

```haskell
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Network.Wai as W
import Web.Firefly

data User = User
  { username::T.Text
  , age::Int
  } deriving (Generic, ToJSON, FromJSON) -- Derive JSON instances

-- A reguler 'ol user
steve :: User
steve = User{username="Steve", age=26}

-- | Get a user by username
getUser :: App W.Response
getUser = do
  uname <- getQuery "username"
  return $ case uname of
             -- The Json constructor signals to serialize the value and respond as "application/json"
             Just "steve" -> toResponse $ Json steve 
             Just name -> toResponse ("Couldn't find user: " `mappend` name, notFound404)
             Nothing -> toResponse ("Please provide a 'username' parameter" :: T.Text, badRequest400)
```

## Should I/Shouldn't I use Firefly?

You should use Firefly if:

- You're intimidated by monads and want to learn more!
- You want to write a hobby project
- You like understanding the stack you're working with (The whole lib is ~300 lines without docs/imports)

Don't use Firefly if:

- You'll have thousands of users
- You want the most performant server possible
- You want to have lots of helper libs available
