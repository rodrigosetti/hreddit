module Main where

import System.Console.CommandLoop
import System.Exit
import Control.Monad (forM_)
import Control.Monad.Trans (lift)

-- | One of the possible sorting for a subreddit
data Sorting = Hot | New | Top | Controversial

-- | A link thing
data Link = Link { domain :: String,
                   name   :: String,
                   title  :: String,
                   url    :: String }

-- | The application's context state
data RedditContext = RedditContext { subreddit :: Maybe String,
                         username  :: Maybe String,
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
                  Command 0 "hot"           cmdSortingHot
                          "Go to the \"hot\" sorting",
                  Command 0 "new"           cmdSortingNew
                          "Go to the \"new\" sorting",
                  Command 0 "top"           cmdSortingTop
                          "Go to the \"top\" sorting",
                  Command 0 "controversial" cmdSortingControversial
                          "Go to the \"controversial\" sorting",
                  Command 1 "login"         cmdLogIn
                          "Login with user name",
                  Command 0 "logout"        cmdLogOut
                          "Logs out",
                  Command 1 "help"          (cmdHelp redditCommands)
                          "Display help about a command or all commands",
                  Command 1 "page"          cmdPageSize
                          "Set or display the current page size",
                  Command 1 "up"            cmdVoteUp
                          "Vote a link number up",
                  Command 1 "down"          cmdVoteDown
                          "Vote a link number down",
                  Command 0 "list"          cmdList
                          "List current page"]
  
-- | loads initial context and start the evaluation loop
main :: IO ()
main = do
    initialContext <- loadInitialContext
    evalExecuteLoop redditCommands initialContext

-- | Loads the initial context. Try to get from a history file, or creates a
--   default one
loadInitialContext :: IO RedditContext
loadInitialContext = return $ RedditContext Nothing Nothing 12 New []

cmdQuit :: CommandFunction RedditContext
cmdQuit _ = lift exitSuccess

cmdNextPage :: CommandFunction RedditContext
cmdNextPage _ = return True

cmdPreviousPage :: CommandFunction RedditContext
cmdPreviousPage _ = return True

cmdFirstPage :: CommandFunction RedditContext
cmdFirstPage _ = return True

cmdSortingHot :: CommandFunction RedditContext
cmdSortingHot _ = return True

cmdSortingNew :: CommandFunction RedditContext
cmdSortingNew _ = return True

cmdSortingTop :: CommandFunction RedditContext
cmdSortingTop _ = return True

cmdSortingControversial :: CommandFunction RedditContext
cmdSortingControversial _ = return True

cmdLogIn :: CommandFunction RedditContext
cmdLogIn _ = return True

cmdLogOut :: CommandFunction RedditContext
cmdLogOut _ = return True

cmdVoteUp :: CommandFunction RedditContext
cmdVoteUp _ = return True

cmdVoteDown :: CommandFunction RedditContext
cmdVoteDown _ = return True

cmdList :: CommandFunction RedditContext
cmdList _ = return True

cmdPageSize :: CommandFunction RedditContext
cmdPageSize _ = return True

