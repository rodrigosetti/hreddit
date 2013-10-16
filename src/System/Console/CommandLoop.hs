module System.Console.CommandLoop ( CommandAction,
                                    Command(..),
                                    evalExecuteLoop,
                                    cmdHelp ) where

import Control.Monad.Error
import Control.Monad.State
import Data.List (isPrefixOf)
import System.Console.Readline

-- | A command action is a contextualized action that can fail and perform IO,
--   therefore it's an IO monad wrapped in a State transformer, wrapped in an
--   Error transformer.
type CommandAction s = ErrorT String (StateT s IO) ()

-- | A command is basically an annotated function with name, description and
--   the maximum parameters it expects.
data Command c = Command { argumentNumber :: Int,
                           cmdName :: String,
                           function :: [String] -> CommandAction c,
                           description :: String }


-- | The evaluation loop reads a user command and performs it, repeating until
--   it reads the end of input.
evalExecuteLoop :: [Command c] -> CommandAction c -> c -> IO ()
evalExecuteLoop commands start =
    evalStateT $ runErrorT start >> loop
  where
    loop = do
        maybeCommand <- liftIO $ readline "> "
        case maybeCommand of
            Nothing -> return ()
            Just "" -> loop
            Just command -> let ws = words command in
                             runErrorT (performCommand commands (head ws) $ tail ws) >>=
                             liftIO . either (putStrLn . (++) "ERROR: ") (\_ -> addHistory command) >>
                             loop

-- | Select the command to run from the available ones. Smartly selects the
--   unique prefix, if exist, and show errors and warnings if the command does
--   not exist, is ambiguous or the argument number is more than necessary.
performCommand :: [Command c] -> String -> [String] -> ErrorT String (StateT c IO) ()
performCommand commands commandName args =
    do command <- getCommand commands commandName
       let argNum = argumentNumber command
       when (length args > argNum) $
            liftIO $ putStrLn $ "warning: \"" ++ commandName ++ "\" takes at most " ++ show argNum ++ " arguments."
       function command args

-- | Displays help information about a single command or all commands
cmdHelp :: [Command c] -> [String] -> CommandAction c
cmdHelp commands (commandName:_) = 
    do command <- getCommand commands commandName
       liftIO $ printCommandInfo command
cmdHelp commands [] = liftIO $ forM_ commands printCommandInfo


-- | get the matched command, or return Nothing - displaying error
--   if the command is ambiguous or unknown
getCommand :: [Command c] -> String -> ErrorT String (StateT c IO) (Command c)
getCommand commands commandName = 
    case length cmds of
        0 -> throwError $ "unknown command \"" ++ commandName ++ "\"."
        1 -> return $ head cmds
        _ -> throwError $ "\"" ++ commandName ++ "\" is ambiguous. Did you mean?: " ++ unwords (map cmdName cmds)
  where
    cmds = filter (isPrefixOf commandName . cmdName) commands

-- | Print information about the command
printCommandInfo :: Command c -> IO ()
printCommandInfo cmd = putStrLn $ cmdName cmd ++ ": " ++ description cmd
