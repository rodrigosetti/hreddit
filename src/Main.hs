module Main where

import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Network.Reddit
import System.Console.CommandLoop
import System.Exit

-- | The application's context state
data RedditContext = RedditContext { subreddit :: Maybe String,
                                     pageSize  :: Int,
                                     sorting   :: Sorting,
                                     links     :: [Link] }

-- | The full list of commands available for the user
redditCommands :: [Command RedditContext]
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
                          "List current page"]

-- | loads initial context and start the evaluation loop
main :: IO ()
main = loadInitialContext >>= evalExecuteLoop redditCommands (load Nothing)

-- | Loads the initial context. Try to get from a history file, or creates a
--   default one
loadInitialContext :: IO RedditContext
loadInitialContext = return $ RedditContext Nothing 12 New []

cmdQuit :: [String] -> CommandAction RedditContext
cmdQuit _ = liftIO exitSuccess

cmdNextPage :: [String] -> CommandAction RedditContext
cmdNextPage _ =
    lift get >>= loadNext . links >> cmdList []
  where
    loadNext [] = throwError "ERROR: could not go to next page from empty page"
    loadNext l = load $  Just $ Right $ name $ last l

cmdPreviousPage :: [String] -> CommandAction RedditContext
cmdPreviousPage _ =
    lift get >>= loadPrev . links >> cmdList []
  where
    loadPrev [] = throwError "ERROR: could not go to previous page from empty page"
    loadPrev (x:_) = load $  Just $ Left $ name x

cmdFirstPage :: [String] -> CommandAction RedditContext
cmdFirstPage _ = load Nothing >> cmdList []

cmdSortingHot :: [String] -> CommandAction RedditContext
cmdSortingHot _ = lift (modify (\c -> c {sorting = Hot})) >> load Nothing >> cmdList []

cmdSortingNew :: [String] -> CommandAction RedditContext
cmdSortingNew _ = lift (modify (\c -> c {sorting = New})) >> load Nothing >> cmdList []

cmdSortingTop :: [String] -> CommandAction RedditContext
cmdSortingTop _ = lift (modify (\c -> c {sorting = Top})) >> load Nothing >> cmdList []

cmdSortingControversial :: [String] -> CommandAction RedditContext
cmdSortingControversial _ = lift (modify (\c -> c {sorting = Controversial})) >> load Nothing >> cmdList []

cmdPageSize :: [String] -> CommandAction RedditContext
cmdPageSize (x:_) = lift (modify (\c -> c {pageSize = read x}))
cmdPageSize [] = lift get >>= liftIO . print . pageSize

cmdSubreddit :: [String] -> CommandAction RedditContext
cmdSubreddit (x:_) = lift (modify (\c -> c {subreddit = Just x})) >> load Nothing >> cmdList []
cmdSubreddit [] = lift get >>= liftIO . putStrLn . fromMaybe "<no subreddit>" . subreddit

cmdList :: [String] -> CommandAction RedditContext
cmdList _ =
    do ctx <- lift get
       let headerDesc = fromMaybe "" (subreddit ctx) ++ "/" ++ show (sorting ctx)
       liftIO $ putStrLn headerDesc
       liftIO $ putStrLn $ replicate (length headerDesc) '='
       liftIO $ printLinks $ links ctx
  where
    printLinks [] = return ()
    printLinks (l:ls) = do putStrLn $ title l
                           putStrLn $ url l
                           putStrLn ""
                           printLinks ls

load :: Maybe (Either String String) -> CommandAction RedditContext
load maybeBeforeOrAfter =
    do ctx <- lift get
       eitherListing <- liftIO $ listing (subreddit ctx) maybeBeforeOrAfter (sorting ctx) (pageSize ctx)
       case eitherListing of
         Left msg -> throwError msg
         Right (Listing ls) -> lift $ modify (\c -> c { links = ls })

