{-# LANGUAGE OverloadedStrings #-}
module Network.Reddit where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (mzero, liftM)
import Data.Aeson hiding (Result)
import Data.ByteString.Lazy hiding (elem, find, map, intersperse)
import Data.List (find, intersperse)
import Data.Maybe (fromMaybe)
import Network.HTTP
import Network.Stream
import Network.URI
import Prelude hiding (concat)
import Data.Monoid (mconcat)

--import Debug.Trace (trace)

-- | One of the possible sorting for a subreddit
data Sorting = Hot | New | Top | Controversial

-- | A link thing
data Link = Link { domain :: String,
                   name   :: String,
                   title  :: String,
                   url    :: String } deriving Show

-- | This data type represent the (relevant) possible data than can came from
--   reddit API calls
data RedditResponse = AuthResult { modhash :: String, cookie :: String } |
                      Listing { modhash :: String, links :: [Link] } |
                      ErrorResponse [String] |
                      HttpError ConnError |
                      ResponseDecodeError
                      deriving Show

instance FromJSON Link where

     parseJSON (Object o) = do d <- o .: "data"
                               Link <$> d .: "domain"
                                    <*> d .: "name"
                                    <*> d .: "title"
                                    <*> d .: "url"
     parseJSON _          = mzero

-- | Since all data is transported via JSON, the RedditResponse type must know
--   how to de-serialize itself from JSON stream.
instance FromJSON RedditResponse where
     parseJSON (Object o) =
        authResult <|> errorResponse <|> listingResp
      where
        authResult    = do jsonObj <- o .: "json"
                           dataObj <- jsonObj .: "data"
                           AuthResult <$> dataObj .: "modhash" <*> dataObj .: "cookie"
        errorResponse = do jsonObj <- o .: "json"
                           errorsArray <- jsonObj .: "errors"
                           return $ ErrorResponse (unwords <$> errorsArray)
        listingResp   = do dataObj <- o .: "data"
                           Listing <$> dataObj .: "modhash" <*> dataObj .: "children"

     parseJSON _      =    mzero

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

-- | Whether or not the given response is a redirect
isRedirect :: Response ty -> Bool
isRedirect r = rspCode r `elem` redirectCodes where
    redirectCodes = [(3,0,1), (3,0,2), (3,0,7)]

-- | Wraps simpleHTTP call with a follow redirects logic
simpleHttpFollowingRedirects :: HStream ty => Int -> Request ty -> IO (Result (Response ty))
simpleHttpFollowingRedirects 0 _ = return $ Left $ ErrorMisc "too many redirects"
simpleHttpFollowingRedirects c req =
    simpleHTTP req >>= either (return . Left) handle
  where
    handle r
        | not $ isRedirect r = return $ Right r
        | otherwise = maybe (return $ Left $ ErrorMisc "HTTP error")
                            (\u -> simpleHttpFollowingRedirects (c-1) req {rqURI = u}) $
                            find ((== HdrLocation) . hdrName) (rspHeaders r) >>= parseURI . hdrValue

apiCall :: Request ByteString -> IO RedditResponse
apiCall req = liftM
              (either HttpError $ fromMaybe ResponseDecodeError . decode . rspBody)
              (simpleHttpFollowingRedirects 10 req)

makeQuery :: [(String, String)] -> String
makeQuery = ('?':) . mconcat . intersperse "&" . map (\(a, b) -> a ++ "=" ++ b)

login :: String -> String -> IO RedditResponse
login username passwd = apiCall $
                        request POST "/api/login" $
                                makeQuery [("rem", "true"),
                                           ("api_type", "json"),
                                           ("user", username),
                                           ("passwd", passwd)]

listing :: Maybe String -> Maybe (Either String String) -> Sorting -> Int -> IO RedditResponse
listing maybeSubreddit maybeBeforeOrAfter sorting limit =
    apiCall $ request GET (subreddit ++ sortingString ++ ".json") $
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

