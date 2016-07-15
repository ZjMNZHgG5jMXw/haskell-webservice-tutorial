{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users"      :> Get '[JSON] [User]
      :<|> "version"    :> Get '[JSON] String
      :<|> "plus"       :> QueryParam "a" Int
                        :> QueryParam "b" Int
                        :> Get '[JSON]    Int
      :<|> "strictplus" :> Capture    "a" Int
                        :> Capture    "b" Int
                        :> Get '[JSON]    Int

startApp :: IO ()
startApp = run 8080 $ logStdoutDev app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> return "version 0.1"
    :<|> plus
    :<|> strictPlus

plus :: (Monad m, Num a) => Maybe a -> Maybe a -> m a
plus ma mb = return $ a + b where
  a = maybe 0 id ma
  b = maybe 0 id mb

strictPlus :: (Monad m, Num a) => a -> a -> m a
strictPlus a b = return $ a + b

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
