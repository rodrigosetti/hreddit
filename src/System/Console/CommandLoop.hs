module System.Console.CommandLoop ( CommandAction,
                                    Command(..),
                                    evalExecuteLoop,
                                    cmdHelp ) where

import Control.Monad.Error
import Control.Monad.State
import Data.List (isPrefixOf)
import System.Console.Haskeline
import System.Console.Haskeline.IO

-- | A command action is a contextualized action that can fail and perform IO,
--   therefore it's an IO monad wrapped in a State transformer, wrapped in an
--   Error transformer.
type CommandAction s m = ErrorT String (StateT s m) ()

-- | A command is basically an annotated function with name, description and
--   the maximum parameters it expects.
data Command c m = Command { argumentNumber :: Int,
                             cmdName :: String,
                             function :: [String] -> CommandAction c m,
                             description :: String }


-- | The evaluation loop reads a user command and performs it, repeating until
--   it reads the end of input.
evalExecuteLoop :: [Command c IO] -> CommandAction c IO -> c -> IO ()
evalExecuteLoop commands start =
    evalStateT $ do _ <- runErrorT start
                    is <- liftIO $ initializeInput defaultSettings
                    loop is
                    liftIO $ closeInput is
  where
    loop is = do
        maybeCommand <- liftIO $ queryInput is $ getInputLine "> "
        case maybeCommand of
            Nothing -> return ()
            Just "" -> loop is
            Just command -> let ws = words command in
                             do r <- runErrorT (performCommand commands (head ws) $ tail ws)
                                liftIO $ either (putStrLn . (++) "ERROR: ") (void . return) r
                                loop is

-- | Select the command to run from the available ones. Smartly selects the
--   unique prefix, if exist, and show errors and warnings if the command does
--   not exist, is ambiguous or the argument number is more than necessary.
performCommand :: MonadIO m => [Command c m] -> String -> [String] -> ErrorT String (StateT c m) ()
performCommand commands commandName args =
    do command <- getCommand commands commandName
       let argNum = argumentNumber command
       when (length args > argNum) $
            liftIO $ putStrLn $ "warning: \"" ++ commandName ++ "\" takes at most " ++ show argNum ++ " arguments."
       function command args

-- | Displays help information about a single command or all commands
cmdHelp :: MonadIO m => [Command c m] -> [String] -> CommandAction c m
cmdHelp commands (commandName:_) = 
    do command <- getCommand commands commandName
       liftIO $ printCommandInfo command
cmdHelp commands [] = liftIO $ forM_ commands printCommandInfo


-- | get the matched command, or return Nothing - displaying error
--   if the command is ambiguous or unknown
getCommand :: MonadIO m => [Command c m] -> String -> ErrorT String (StateT c m) (Command c m)
getCommand commands commandName = 
    case length cmds of
        0 -> throwError $ "unknown command \"" ++ commandName ++ "\"."
        1 -> return $ head cmds
        _ -> throwError $ "\"" ++ commandName ++ "\" is ambiguous. Did you mean?: " ++ unwords (map cmdName cmds)
  where
    cmds = filter (isPrefixOf commandName . cmdName) commands

-- | Print information about the command
printCommandInfo :: Command c m -> IO ()
printCommandInfo cmd = putStrLn $ cmdName cmd ++ ": " ++ description cmd

