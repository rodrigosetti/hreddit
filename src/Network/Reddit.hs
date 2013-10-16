{-# LANGUAGE OverloadedStrings #-}
module Network.Reddit (Sorting(..),
                       Page(..),
                       Link(domain, name, title, url),
                       Listing(..),
                       listing) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Data.Aeson hiding (Result)
import Data.ByteString.Lazy hiding (elem, find, map, intersperse, filter, isPrefixOf)
import Network.HTTP
import Network.HTTP.Utils
import Network.URI
import Prelude hiding (concat)
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- | One of the possible sorting for a subreddit
data Sorting = Hot | New | Top | Controversial deriving Show

instance Read Sorting where
    readsPrec _ str =
        case match (map toLower str) of
            (x:_)   -> [(x, "")]
            _       -> [(New, "")]
      where
        values = [("hot", Hot), ("new", New),
            ("top", Top), ("controversial",Controversial)]
        match :: String -> [Sorting]
        match value = map snd $ filter (isPrefixOf value . fst) values

-- | Pagination values
data Page = First | Before String | After String

-- | A Link thing
data Link = Link { domain :: String,
                   name   :: String,
                   title  :: String,
                   url    :: String } deriving Show

instance FromJSON Link where
    parseJSON (Object o) = do d <- o .: "data"
                              Link <$> d .: "domain"
                                   <*> d .: "name"
                                   <*> d .: "title"
                                   <*> d .: "url"
    parseJSON _          = mzero

data Listing = Listing [Link] deriving Show

instance FromJSON Listing where
    parseJSON (Object o) = do d <- o .: "data"
                              Listing <$> d .: "children"
    parseJSON _          = mzero


-- | A (internal) Reddit API request constructor
request :: RequestMethod -> String -> String -> Request ByteString
request method requestPath queryString =
    Request { rqURI = uri ,
              rqMethod = method,
              rqHeaders = [Header HdrContentLength "0",
                           Header HdrUserAgent "User-Agent: hreddit/1.0"],
              rqBody = empty }
  where
    uri = URI "http"
              (Just (URIAuth "" "api.reddit.com" ""))
              requestPath
              queryString
              ""

-- | Get a page of Reddit listing
listing :: Maybe String -> Page -> Sorting -> Int -> ErrorT String IO Listing
listing maybeSubreddit page sorting limit =
    jsonAPICall $ request GET (subreddit ++ sortingString ++ ".json") $
                       makeQuery $ ("limit", show $ restrict 0 100 limit):otherQueryParams
  where
    restrict a b = max a . min b
    subreddit = maybe "/" (\n -> "/r/" ++ n ++ "/") maybeSubreddit
    otherQueryParams = case page of
                            First -> []
                            Before fullname -> [("before", fullname)]
                            After fullname -> [("after", fullname)]
    sortingString = case sorting of
                        Hot -> "hot"
                        New -> "new"
                        Top -> "top"
                        Controversial -> "controversial"

