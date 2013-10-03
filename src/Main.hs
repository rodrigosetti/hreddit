module Main where

import Control.Monad.State
import System.Console.Readline
import Data.List (isPrefixOf)
import System.Exit

-- | One of the possible sorting for a subreddit
data Sorting = Hot | New | Top | Controversial

-- | A link thing
data Link = Linkg { domain :: String,
                    name   :: String,
                    title  :: String,
                    url    :: String }

-- | The application's context state
data Context = Context { subreddit :: Maybe String,
                         username  :: Maybe String,
                         pageSize  :: Int,
                         sorting   :: Sorting,
                         links     :: [Link] }

-- | A command function takes a list of arguments, and runs a IO
--   monad with Context state, returning whether or not it was successful
type CommandFunction = [String] -> StateT Context IO Bool

-- | A command is basically an annotated function with name, description and
--   the maximum parameters it expects.
data Command = Command { argumentNumber :: Int,
                         cmdName :: String,
                         function :: CommandFunction,
                         description :: String }

-- | The full list of commands available for the user
commands :: [Command]
commands = [Command 0 "quit"          cmdQuit
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
            Command 1 "help"          cmdHelp
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
    evalStateT evalLoop initialContext

-- | Loads the initial context. Try to get from a history file, or creates a
--   default one
loadInitialContext :: IO Context
loadInitialContext = return $ Context Nothing Nothing 12 New []

-- | The evaluation loop reads a user command and perform it
evalLoop :: StateT Context IO ()
evalLoop = do
    maybeCommand <- lift $ readline "> "
    case maybeCommand of
        Nothing -> return ()
        Just "" -> evalLoop
        Just command -> do let ws = words command
                           success <- performCommand (head ws) (tail ws)
                           when success $ lift $ addHistory command
                           evalLoop

-- | Select the command to run from the available ones. Smartly selects the
--   unique prefix, if exist, and show errors and warnings if the command does
--   not exist, is ambiguous or the argument number is more than necessary.
performCommand :: String -> [String] -> StateT Context IO Bool
performCommand commandName args =
    case length cmds of
        0 -> do lift $ putStrLn $ "unknown command \"" ++ commandName ++ "\""
                return False
        1 -> do let cmd = head cmds
                    argNum = argumentNumber cmd
                when (length args > argNum) $
                     lift $ putStrLn $ "warning: \"" ++ commandName ++ "\" takes at most " ++ show argNum ++ " arguments."
                function cmd args
        _ -> do lift $ putStrLn $ "\"" ++ commandName ++ "\" is ambiguous. Did you mean?: " ++ unwords (map cmdName cmds)
                return False
  where
    cmds = matchedCommands commandName

-- | Return all the commands matched by the command name prefix
matchedCommands :: String -> [Command]
matchedCommands commandName = filter (isPrefixOf commandName . cmdName) commands

cmdQuit :: CommandFunction
cmdQuit _ = lift exitSuccess

cmdNextPage :: CommandFunction
cmdNextPage _ = return True

cmdPreviousPage :: CommandFunction
cmdPreviousPage _ = return True

cmdFirstPage :: CommandFunction
cmdFirstPage _ = return True

cmdSortingHot :: CommandFunction
cmdSortingHot _ = return True

cmdSortingNew :: CommandFunction
cmdSortingNew _ = return True

cmdSortingTop :: CommandFunction
cmdSortingTop _ = return True

cmdSortingControversial :: CommandFunction
cmdSortingControversial _ = return True

cmdLogIn :: CommandFunction
cmdLogIn _ = return True

cmdLogOut :: CommandFunction
cmdLogOut _ = return True

cmdVoteUp :: CommandFunction
cmdVoteUp _ = return True

cmdVoteDown :: CommandFunction
cmdVoteDown _ = return True

cmdList :: CommandFunction
cmdList _ = return True

cmdPageSize :: CommandFunction
cmdPageSize _ = return True

cmdHelp :: CommandFunction
cmdHelp (commandName:_) = 
    case length cmds of
        0 -> do lift $ putStrLn $ "unknown command \"" ++ commandName ++ "\"."
                lift $ putStrLn "Use \"help\" without arguments to see the list of commands"
                return False
        1 -> do let cmd = head cmds
                lift $ printCommandInfo cmd
                return True
        _ -> do lift $ putStrLn $ "\"" ++ commandName ++ "\" is ambiguous. Did you mean?: " ++ unwords (map cmdName cmds)
                lift $ putStrLn "Use \"help\" without arguments to see the list of commands"
                return False
  where
    cmds = matchedCommands commandName
cmdHelp [] = lift $ forM_ commands printCommandInfo >> return True

-- | Print information about the command
printCommandInfo :: Command -> IO ()
printCommandInfo cmd = putStrLn $ cmdName cmd ++ ": " ++ description cmd

