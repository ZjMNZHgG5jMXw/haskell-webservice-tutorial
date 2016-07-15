{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Github where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.Client
import Network.HTTP.Client        ( Manager, newManager )
import Network.HTTP.Client.TLS    ( tlsManagerSettings )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )

type API = "search" :> "repositories" :> Header "User-Agent" String :> QueryParam "q" String :> Get '[JSON] SearchResult

api :: Proxy API
api = Proxy

searchClient :: Maybe String -> Maybe String -> Manager -> BaseUrl -> ClientM SearchResult
searchClient = client api

search :: String -> IO SearchResult
search query = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.github.com" 443 ""
  res <- runExceptT $ searchClient (Just "Awesome-Octocat-App") (Just query) manager baseUrl
  case res of
    Left err            -> fail $ "Error: " ++ show err
    Right searchResult  -> return searchResult

data SearchResult = SearchResult
  { total_count         :: Maybe Int
  , incomplete_results  :: Maybe Bool
  , items               :: [SearchItem]
  } deriving Show

data SearchItem = SearchItem
  { id                :: Maybe Int
  , name              :: Maybe String
  , full_name         :: Maybe String
  , owner             :: Maybe SearchItemOwner
  , private           :: Maybe Bool
  , html_url          :: Maybe String
  , description       :: Maybe String
  , fork              :: Maybe Bool
  , url               :: Maybe String
  , created_at        :: Maybe String -- Date
  , updated_at        :: Maybe String -- Date
  , pushed_at         :: Maybe String -- Date
  , homepage          :: Maybe String
  , size              :: Maybe Int
  , stargazers_count  :: Maybe Int
  , watchers_count    :: Maybe Int
  , language          :: Maybe String
  , forks_count       :: Maybe Int
  , open_issues_count :: Maybe Int
  , master_branch     :: Maybe String
  , default_branch    :: Maybe String
  , score             :: Maybe Double
  } deriving Show

data SearchItemOwner = SearchItemOwner
  { login               :: Maybe String
  --, id                  :: Maybe Int
  , avatar_url          :: Maybe String
  , gravatar_id         :: Maybe String
  --, url                 :: Maybe String
  , received_events_url :: Maybe String
  --, type                :: Maybe String
  } deriving Show

$(deriveJSON defaultOptions ''SearchItemOwner)
$(deriveJSON defaultOptions ''SearchItem)
$(deriveJSON defaultOptions ''SearchResult)
