module Main where

import Lexer
import Parser
import Entities
import qualified Simulator

import Text.Show.Pretty

import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative

import Data.List (isPrefixOf)

import System.Cmd
import System.Environment
import System.Console.Haskeline

-------------------------------------------------------------------------------
-- REPL
-------------------------------------------------------------------------------

type Repl a = StateT IState (InputT IO) a

data IState = IState
  { _tenv :: Maybe [Decl]
  , _curFile :: Maybe FilePath
  }

initState :: IState
initState = IState Nothing Nothing

runRepl :: Repl a -> IO IState
runRepl f = runInputT defaultSettings (execStateT f initState)

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

load :: String -> Repl ()
load arg = do
  liftIO $ putStrLn $ "Loading " ++ arg
  file arg
  modify $ \s -> s { _curFile = Just arg }

reload :: Repl ()
reload = do
  fname <- gets _curFile
  case fname of
    Just fname' -> handleCommand "load" [fname']
    Nothing -> liftIO $ putStrLn "No such file"

-- Run the simulation
run :: Repl ()
run = do
  env <- gets _tenv
  case env of
    Just decls -> do
      output <- liftIO $ Simulator.simulate decls
      case output of
        Left err -> liftIO $ putStrLn $ "Simulation Error: " ++ (show err)
        Right res -> do
          {-liftIO $ putStrLn (ppShow res)-}
          liftIO $ putStrLn "Done."

    Nothing -> liftIO $ putStrLn "No file loaded."

-- Run the simulation
dump :: Repl ()
dump = do
  env <- gets _tenv
  case env of
    Just env' -> liftIO $ putStrLn (ppShow env')
    Nothing -> liftIO $ putStrLn "No file loaded."

help :: Repl ()
help = liftIO $ do
  putStrLn ":run          Run the loaded program"
  putStrLn ":reload       Run the active file"
  putStrLn ":show         Dump the AST of active file"
  putStrLn ":load <file>  Load a program from file"

-------------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------------

-- evaluate a module
modl :: FilePath -> String -> Repl ()
modl fname source = do
  mod <- liftIO $ parseFile fname
  case mod of
    Left  err -> liftIO $ print err
    Right res -> modify $ \s -> s { _tenv = Just res }

-- evaluate an expression
exec :: String -> Repl ()
exec source = return ()

file :: FilePath -> Repl ()
file fname = do
  contents <- liftIO $ readFile fname
  modl fname contents

handleCommand :: String -> [String] -> Repl ()
handleCommand cmd args
    | "sh"  `isPrefixOf` cmd = dump
    | "ru"  `isPrefixOf` cmd = run
    | "r"   `isPrefixOf` cmd = reload
    | "h"   `isPrefixOf` cmd = help
    | length args == 0 = liftIO $ putStrLn "Not enough arguments"
    | "l"  `isPrefixOf` cmd = load arg
    | otherwise = liftIO $ putStrLn "Unknown command"
  where
    arg = head args

repl :: Repl ()
repl = do
  minput <- lift $ getInputLine "Agents> "
  case minput of
    Nothing -> lift $ outputStrLn "Goodbye."

    Just (':' : cmds) -> do
      let (cmd:args) = words cmds
      handleCommand cmd args
      repl

    Just input -> do
      exec input
      repl

main :: IO ()
main = do
  preamble <- readFile "logo"
  putStrLn preamble

  args <- getArgs
  case args of
    []      -> void $ runRepl repl
    [fname] -> void $ runRepl (file fname)
    _ -> putStrLn "invalid arguments"

test :: IO ()
test = void $ runRepl $ do
  file "simple.world"
  -- dump
  run
