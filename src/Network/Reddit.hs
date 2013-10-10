{-# LANGUAGE OverloadedStrings #-}
module Network.Reddit where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, liftM)
import Data.Aeson hiding (Result)
import Data.ByteString.Lazy hiding (elem, find, map, intersperse)
import Network.HTTP
import Network.HTTP.Utils
import Network.Stream
import Network.URI
import Prelude hiding (concat)

--import Debug.Trace (trace)

-- | One of the possible sorting for a subreddit
data Sorting = Hot | New | Top | Controversial

-- | A link thing
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


-- | A Reddit API request
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
listing :: Maybe String -> Maybe (Either String String) -> Sorting -> Int -> IO (Either String Listing)
listing maybeSubreddit maybeBeforeOrAfter sorting limit =
    jsonAPICall $ request GET (subreddit ++ sortingString ++ ".json") $
                       makeQuery $ ("limit", show $ restrict 0 100 limit):otherQueryParams
  where
    restrict a b = max a . min b
    subreddit = maybe "/" (\n -> "/r/" ++ n ++ "/") maybeSubreddit
    otherQueryParams = case maybeBeforeOrAfter of
                            Nothing -> []
                            Just (Left fullname) -> [("before", fullname)]
                            Just (Right fullname) -> [("after", fullname)]
    sortingString = case sorting of
                        Hot -> "hot"
                        New -> "new"
                        Top -> "top"
                        Controversial -> "controversial"

