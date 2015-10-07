module Main where

import Control.Applicative ((<|>))
import Control.Exception (catch, IOException)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe, fromJust)
import Network.Reddit
import System.Console.CommandLoop
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)


-- | The application's context state
data RedditContext = RedditContext { subreddit :: Maybe String,
                                     pageSize  :: Int,
                                     sorting   :: Sorting,
                                     links     :: [Link] }

-- | The full list of commands available for the user
redditCommands :: [Command RedditContext IO]
redditCommands = [Command 0 "quit"          cmdQuit
                          "Exit the program",
                  Command 0 "next"          cmdNextPage
                          "Go to next page and list",
                  Command 0 "previous"      cmdPreviousPage
                          "Go to previous page and list",
                  Command 0 "first"         cmdFirstPage
                          "Go to first page and list",
                  Command 1 "subreddit"     cmdSubreddit
                          "Change or show subredit",
                  Command 0 "hot"           cmdSortingHot
                          "Go to the \"hot\" sorting",
                  Command 0 "new"           cmdSortingNew
                          "Go to the \"new\" sorting",
                  Command 0 "top"           cmdSortingTop
                          "Go to the \"top\" sorting",
                  Command 0 "controversial" cmdSortingControversial
                          "Go to the \"controversial\" sorting",
                  Command 1 "help"          (cmdHelp redditCommands)
                          "Display help about a command or all commands",
                  Command 1 "page"          cmdPageSize
                          "Set or display the current page size",
                  Command 0 "list"          cmdList
                          "List current page",
                  Command 1 "open"          cmdOpen
                          "Open link ID in default browser"
                 ]

-- | loads initial context and start the evaluation loop
main :: IO ()
main = loadInitialContext >>= evalExecuteLoop redditCommands (load First)

-- | Loads the initial context. Try to get either from a command line arguments, or from history file, or creates a
--   default one
loadInitialContext :: IO RedditContext
loadInitialContext = do
    subreddit'  <- getSubredditField
    sorting'    <- getSortingField
    return $ RedditContext subreddit' 12 sorting' []
  where
    getSubredditField :: IO (Maybe String)
    getSubredditField = do
        args    <- getArgs
        history <- lines <$> getHistoryFile
        return $ fromArgs args <|> fromHistory history <|> defaultValue
      where
        fromArgs (arg:_)
          | null (parseArg arg) = Nothing
          | otherwise           = Just $ parseArg arg
        fromArgs _              = Nothing
        fromHistory             = join . (readMaybe =<<) . maybeIndex 0
        defaultValue            = Nothing

        parseArg = takeWhile ('/' /=)

    getSortingField :: IO Sorting
    getSortingField = do
        args <- getArgs
        history <- lines <$> getHistoryFile
        return $ fromJust $ fromArgs args <|> fromHistory history <|> defaultValue
      where
        fromArgs (arg:_)
          | null (parseArg arg) = Nothing
          | otherwise           = Just $ read $ tail $ parseArg arg
        fromArgs _              = Nothing
        fromHistory             = (read <$>) . maybeIndex 1
        defaultValue            = Just Hot

        parseArg = dropWhile ('/' /=)

    getHistoryFile = catch (readFile =<< historyFileLocation)
        ((\_ -> return "") :: IOException -> IO String)

maybeIndex :: Int -> [a] -> Maybe a
maybeIndex i list
    | (i < length list) && i >= 0 = Just $ list !! i
    | otherwise = Nothing


cmdQuit :: [String] -> CommandAction RedditContext IO
cmdQuit _ = do
    ctx <- lift get
    liftIO (writeHistoryFile ctx >> exitSuccess)

cmdNextPage :: [String] -> CommandAction RedditContext IO
cmdNextPage _ =
    lift get >>= loadNext . links >> cmdList []
  where
    loadNext [] = throwE "could not go to next page from empty page"
    loadNext l = load $ After $ name $ last l

cmdPreviousPage :: [String] -> CommandAction RedditContext IO
cmdPreviousPage _ =
    lift get >>= loadPrev . links >> cmdList []
  where
    loadPrev [] = throwE "could not go to previous page from empty page"
    loadPrev (x:_) = load $ Before $ name x

cmdFirstPage :: [String] -> CommandAction RedditContext IO
cmdFirstPage _ = load First >> cmdList []

cmdSortingHot :: [String] -> CommandAction RedditContext IO
cmdSortingHot _ = lift (modify (\c -> c {sorting = Hot})) >> load First >> cmdList []

cmdSortingNew :: [String] -> CommandAction RedditContext IO
cmdSortingNew _ = lift (modify (\c -> c {sorting = New})) >> load First >> cmdList []

cmdSortingTop :: [String] -> CommandAction RedditContext IO
cmdSortingTop _ = lift (modify (\c -> c {sorting = Top})) >> load First >> cmdList []

cmdSortingControversial :: [String] -> CommandAction RedditContext IO
cmdSortingControversial _ = lift (modify (\c -> c {sorting = Controversial})) >> load First >> cmdList []

cmdPageSize :: [String] -> CommandAction RedditContext IO
cmdPageSize (x:_) = lift (modify (\c -> c {pageSize = read x}))
cmdPageSize [] = lift get >>= liftIO . print . pageSize

cmdSubreddit :: [String] -> CommandAction RedditContext IO
cmdSubreddit (x:_) = lift (modify (\c -> c {subreddit = Just x})) >> load First >> cmdList []
cmdSubreddit [] = lift get >>= liftIO . putStrLn . fromMaybe "<no subreddit>" . subreddit

cmdOpen :: [String] -> CommandAction RedditContext IO
cmdOpen (x:_) = case readMaybe x of
      Nothing -> throwE "specified link ID not valid number"
      Just x' ->
       if x' > 0
         then do
           c <- lift get
           if length (links c) >= x'
             then liftIO $ openBrowserOn $ url $ flip (!!) (x' - 1) $ links c
             else throwE "link ID too large"
         else throwE "link ID less than 1"
cmdOpen _ = throwE "no link ID given to 'open' command"

cmdList :: [String] -> CommandAction RedditContext IO
cmdList _ =
    do ctx <- lift get
       let headerDesc = fromMaybe "" (subreddit ctx) ++ "/" ++ show (sorting ctx)
       liftIO $ do putStrLn headerDesc
                   putStrLn $ replicate (length headerDesc) '='
                   printLinks (links ctx) 1 
  where
    printLinks :: [Link] -> Int -> IO ()
    printLinks [] _ = return ()
    printLinks (l:ls) i = do putStrLn $ show i ++ ". " ++ title l
                             putStrLn $ url l
                             putStrLn ""
                             printLinks ls (i+1)

load :: Page -> CommandAction RedditContext IO
load page = do ctx <- lift get
               liftIO (runExceptT $ listing (subreddit ctx) page (sorting ctx) $ pageSize ctx) >>=
                either throwE (\(Listing ls) -> lift $ modify $ \c -> c { links = ls })

-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ()
openBrowserOn = trybrowsers browsers
    where
      trybrowsers [] _ = return () -- unable to open web browser
      trybrowsers (b:bs) u = do
        (e,_,_) <- readProcessWithExitCode b [u] ""
        case e of
          ExitSuccess -> return ()
          ExitFailure _ -> trybrowsers bs u
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["start"] -- needs testing
               | os=="linux"   = ["xdg-open","google-chrome","firefox"]
               | otherwise     = []

writeHistoryFile :: RedditContext -> IO ()
writeHistoryFile ctx = do
    location <- historyFileLocation
    writeFile location $ (show . subreddit) ctx ++
                            '\n' : (show . sorting) ctx ++ "\n"
historyFileLocation :: IO FilePath
historyFileLocation = flip (++) "/.hreddit_history" `fmap` getHomeDirectory
