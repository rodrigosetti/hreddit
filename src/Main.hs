module Main where

import Control.Monad (forM_)
import Control.Monad.Trans (lift)
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
main = loadInitialContext >>= evalExecuteLoop redditCommands

-- | Loads the initial context. Try to get from a history file, or creates a
--   default one
loadInitialContext :: IO RedditContext
loadInitialContext = return $ RedditContext Nothing 12 New []

cmdQuit :: CommandFunction RedditContext
cmdQuit _ = lift exitSuccess

cmdNextPage :: CommandFunction RedditContext
cmdNextPage _ = return True

cmdPreviousPage :: CommandFunction RedditContext
cmdPreviousPage _ = return True

cmdFirstPage :: CommandFunction RedditContext
cmdFirstPage _ = load Nothing >> cmdList []

cmdSortingHot :: CommandFunction RedditContext
cmdSortingHot _ = modify (\c -> c {sorting = Hot}) >> load Nothing >> cmdList []

cmdSortingNew :: CommandFunction RedditContext
cmdSortingNew _ = modify (\c -> c {sorting = New}) >> load Nothing >> cmdList []

cmdSortingTop :: CommandFunction RedditContext
cmdSortingTop _ = modify (\c -> c {sorting = Top}) >> load Nothing >> cmdList []

cmdSortingControversial :: CommandFunction RedditContext
cmdSortingControversial _ = modify (\c -> c {sorting = Controversial}) >> load Nothing >> cmdList []

cmdPageSize :: CommandFunction RedditContext
cmdPageSize (x:_) = modify (\c -> c {pageSize = read x}) >> return True
cmdPageSize [] = do ctx <- get
                    lift $ print $ pageSize ctx
                    return True

cmdSubreddit :: CommandFunction RedditContext
cmdSubreddit (x:_) = modify (\c -> c {subreddit = Just x}) >> load Nothing >> cmdList []
cmdSubreddit [] = do ctx <- get
                     lift $ putStrLn $ fromMaybe "<no subreddit>" $ subreddit ctx
                     return True

cmdList :: CommandFunction RedditContext
cmdList _ =
    do ctx <- get
       lift $ putStrLn $ (fromMaybe "" $ subreddit ctx) ++ "/" ++ show (sorting ctx)
       lift $ putStrLn "------------------"
       lift $ printLinks $ links ctx
       return True
  where
    printLinks [] = return ()
    printLinks (l:ls) = do putStrLn $ title l
                           putStrLn $ url l
                           putStrLn ""
                           printLinks ls

load :: Maybe (Either String String) -> StateT RedditContext IO Bool
load maybeBeforeOrAfter =
    do ctx <- get
       eitherListing <- lift $ listing (subreddit ctx) maybeBeforeOrAfter (sorting ctx) (pageSize ctx)
       case eitherListing of
         Left msg -> lift $ putStrLn msg >> return False
         Right (Listing ls) -> do modify (\c -> c { links = ls })
                                  return True

