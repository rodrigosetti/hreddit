module Network.HTTP.Utils (isRedirect,
                           simpleHttpFollowingRedirects,
                           jsonAPICall,
                           makeQuery) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson hiding (Result)
import Data.ByteString.Lazy hiding (elem, find, map, intersperse)
import Data.List (find, intersperse)
import Network.HTTP
import Network.Stream
import Network.URI

-- | Whether or not the given response is a redirect
isRedirect :: Response ty -> Bool
isRedirect r = rspCode r `elem` redirectCodes where
    redirectCodes = [(3,0,1), (3,0,2), (3,0,7)]

-- | Wraps simpleHTTP call with a follow redirects logic
simpleHttpFollowingRedirects :: HStream s => Int -> Request s -> IO (Result (Response s))
simpleHttpFollowingRedirects 0 _ = return $ Left $ ErrorMisc "too many redirects"
simpleHttpFollowingRedirects c req =
    simpleHTTP req >>= either (return . Left) handle
  where
    handle r
        | not $ isRedirect r = return $ Right r
        | otherwise = maybe (return $ Left $ ErrorMisc "HTTP error")
                            (\u -> simpleHttpFollowingRedirects (c-1) req {rqURI = u}) $
                            find ((== HdrLocation) . hdrName) (rspHeaders r) >>= parseURI . hdrValue

jsonAPICall :: FromJSON a => Request ByteString -> ExceptT String IO a
jsonAPICall req = do eitherResult <- liftIO $ simpleHttpFollowingRedirects 10 req
                     case eitherResult of
                        Left ErrorReset -> throwE "connection reset"
                        Left ErrorClosed -> throwE "connection closed"
                        Left (ErrorParse s) -> throwE $ "HTTP parsing: " ++ s
                        Left (ErrorMisc s) -> throwE s
                        Right r -> maybe (throwE "invalid response from server") return $ decode $ rspBody r

makeQuery :: [(String, String)] -> String
makeQuery = ('?':) . mconcat . intersperse "&" . map (\(a, b) -> a ++ "=" ++ b)

