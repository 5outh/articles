---
title: Posting to Twitter via HTTP in haskell
author: Ben Kovach
tags: twitter, http, haskell
---

When working on [rapcandy](https://twitter.com/_rapcandy), I had a bit of trouble getting the twitter connector bit working. I'll detail here what worked -- if you need to connect to Twitter via one of your Haskell applications, this should help you out.

### Setting up a Twitter Application

In order to interact with Twitter programmatically, you'll first need to set up an application via [dev.twitter.com](http://dev.twitter.com). You can create a new app here and generate new API keys for it. Once you do this, you'll be able to *read* from twitter. If you want to write to twitter (like I did, with [\@_rapcandy](https://twitter.com/_rapcandy)), you can go to the *API Keys* tab, click "modify app permissions" and give it write access. You can then generate new API keys which will permit writing. 

Copy down your API key, API secret, Access token, and Access token secret into a JSON file called `config.json` that looks like this:

```json
{
    "apiKey": "<api key>",
    "apiSecret": "<api secret>",
    "userKey": "<user key>",
    "userSecret": "<user secret>"
}
```

Now everything's in place to be able to start interacting with Twitter via Haskell.


### Posting to Twitter

We'll be using `aeson`, `HTTP`, and `authenticate-oauth` (Twitter uses OAuth to authenticate its users) to handle the transaction. A bit of boilerplate:

```haskell
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

import           Control.Monad
import           Data.Aeson
import           GHC.Generics
import           Data.ByteString
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as BL
import qualified Network.HTTP.Base       as HTTP
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Web.Authenticate.OAuth
```

First thing's first, we need to be able to pull in our config file in order to access the keys for our application. We "magically" do this using `Aeson`, deriving `Generic` and making automatic instances of `{To, From}JSON` for our user-defined `Config` type.

```haskell
data Config = Config {
    apiKey       :: String,
    apiSecret    :: String,
    userKey      :: String,
    userSecret   :: String
  } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config
```

We can pull in a `Config` from a file with a basic function `configFromFile`:

```haskell
configFromFile :: FilePath -> IO (Either String Config)
configFromFile path = do
  contents <- BL.readFile path
  return $ eitherDecode contents
```

Now calling `configFromFile "config.json"` should return something like `Right (Config{...})`. Now we can start authenticating requests to the Twitter API. The following function is adapted from the [Yesod source code](http://hackage.haskell.org/package/yesod-auth-0.7.2/docs/src/Yesod-Auth-OAuth.html) to be less specific to Yesod:

```haskell
oauthTwitter :: ByteString -> ByteString -> OAuth
oauthTwitter key secret =
  newOAuth { oauthServerName      = "twitter"
           , oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
           , oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
           , oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
           , oauthSignatureMethod = HMACSHA1
           , oauthConsumerKey     = key
           , oauthConsumerSecret  = secret
           , oauthVersion         = OAuth10a
           }
```

Here we pass in our OAuth consumer key and secret to build an `OAuth`; these correspond to the Twitter API key/secret, and is one half of what we need to fully authenticate Twitter requests. The other half is a `Credential`, which we can build with `newCredential` using our *user* key and secret. We can fully sign an arbitrary request using a `Config`: 

```haskell
signWithConfig :: Config -> Request -> IO Request
signWithConfig Config{..} = signOAuth
  (oauthTwitter (B.pack apiKey) (B.pack apiSecret))
  (newCredential (B.pack userKey) (B.pack userSecret))
```

Now all we have to do is actually *send* a request (post a status!), which is simple but took me a while to finagle into place. There are three things to keep in mind here:

1. We must `urlEncode` the status we want to send.
2. We must `POST`; not `GET`.
3. We must use `tlsManagerSettings` to enable TLS for our request (otherwise, the request won't go through)

```haskell
tweet :: Config -> String -> IO (Response BL.ByteString)
tweet config status = do
  url <- parseUrl $ "https://api.twitter.com/1.1/statuses/update.json?status=" ++ HTTP.urlEncode status
  req <- signWithConfig config url{ method = "POST" }
  manager <- newManager tlsManagerSettings
  httpLbs req manager
```

Using this code and a sufficiently permissive Twitter application, you should be able to adapt this code to send requests to any of the [Twitter REST API endpoints](https://dev.twitter.com/docs/api/1.1) from Haskell.

Ben