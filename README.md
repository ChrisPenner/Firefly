# Firefly
[![Build Status](https://travis-ci.org/ChrisPenner/Firefly.svg?branch=master)](https://travis-ci.org/ChrisPenner/Firefly)

- [Example App](./firefly-example/app/Main.hs)
- [Hackage Docs](http://hackage.haskell.org/package/firefly-0.1.0.0/docs/Web-Firefly.html)

Firefly is a dead simple http framework written in Haskell.

It strives for simplicity in implementation (and in use).
It's great for people learning Haskell, fiddling with Monads,
or who just need a really simple server for something.

Here's a simple app:

```haskell
{-# language OverloadedStrings #-}
import Web.Firefly
import qualified Data.Text as T

main :: IO ()
main = run 3000 app

app :: App ()
app = do
  route "/hello" helloHandler

-- | Get the 'name' query param from the url, if it doesn't exist use 'Stranger'
helloHandler :: Handler T.Text
helloHandler = do
  name <- fromMaybe "Stranger" <$> getQuery "name"
  return $ "Hello " `T.append` name

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
             -- We can use a tuple to pass a status alongside the response body
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

## Troubleshooting

### pcre.h not found

Seeing something like this?
```
...stack/regex-pcre-0.94.4/Wrap.hsc:148:10: fatal error: 'pcre.h' file not found
    #include <pcre.h>
             ^~~~~~~~
    1 error generated.
```

Firefly uses regex; and requires certain c-libs to be installed. The easiest way to fix this is to install Nix:

[Get Nix](https://nixos.org/nix/)

*or* run:

`curl https://nixos.org/nix/install | sh`

Then add the following to your project's `stack.yaml`:

```
nix:
  enable: true
  packages:
    - libcxx
    - icu
    - gcc
    - ncurses
    - pcre
    - zlib
```
