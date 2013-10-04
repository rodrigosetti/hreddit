module System.Console.CommandLoop where

import Control.Monad.State
import System.Console.Readline
import Data.List (isPrefixOf)

-- | A command function takes a list of arguments, and runs a IO
--   monad with Context state, returning whether or not it was successful
type CommandFunction c = [String] -> StateT c IO Bool

-- | A command is basically an annotated function with name, description and
--   the maximum parameters it expects.
data Command c = Command { argumentNumber :: Int,
                           cmdName :: String,
                           function :: CommandFunction c,
                           description :: String }


-- | The evaluation loop reads a user command and perform it
--
evalExecuteLoop :: [Command c] -> c -> IO ()
evalExecuteLoop commands = evalStateT loop
  where
    loop = do
        maybeCommand <- lift $ readline "> "
        case maybeCommand of
            Nothing -> return ()
            Just "" -> loop
            Just command -> do let ws = words command
                               success <- performCommand commands (head ws) (tail ws)
                               when success $ lift $ addHistory command
                               loop

-- | Select the command to run from the available ones. Smartly selects the
--   unique prefix, if exist, and show errors and warnings if the command does
--   not exist, is ambiguous or the argument number is more than necessary.
performCommand :: [Command c] -> String -> [String] -> StateT c IO Bool
performCommand commands commandName args =
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
    cmds = matchedCommands commands commandName

-- | Return all the commands matched by the command name prefix
matchedCommands :: [Command c] -> String -> [Command c]
matchedCommands commands commandName = filter (isPrefixOf commandName . cmdName) commands

cmdHelp :: [Command c] -> CommandFunction c
cmdHelp commands (commandName:_) = 
    case length cmds of
        0 -> do lift $ putStrLn $ "unknown command \"" ++ commandName ++ "\"."
                return False
        1 -> do let cmd = head cmds
                lift $ printCommandInfo cmd
                return True
        _ -> do lift $ putStrLn $ "\"" ++ commandName ++ "\" is ambiguous. Did you mean?: " ++ unwords (map cmdName cmds)
                return False
  where
    cmds = matchedCommands commands commandName
cmdHelp commands [] = lift $ forM_ commands printCommandInfo >> return True

-- | Print information about the command
printCommandInfo :: Command c -> IO ()
printCommandInfo cmd = putStrLn $ cmdName cmd ++ ": " ++ description cmd

