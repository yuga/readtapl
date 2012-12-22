{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception     as E
import qualified Data.ByteString       as S
import qualified System.Console.GetOpt as O
import qualified System.Exit           as X
import qualified System.IO             as IO
import qualified System.IO.Error       as IO

import Control.Applicative
import Control.Monad
import Core
import Parser
import System.Environment    (getArgs, getProgName)
import Text.Parsec

--import Debug.Trace

--
-- Entry
--

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    let header = "Usage: " ++ prog ++ " [OPTIONS] FILE\n"
        usage = O.usageInfo header argspec
    case O.getOpt O.Permute argspec args of
        (o, [],  []) -> if optInteractive (foldl (.) id o defaultConfig)
                        then processPrompt
                        else die ("FILE is not specified\n" ++ usage) 1
        (o, f:_, []) -> process $ setFile f $ foldl (.) id o defaultConfig
        (_, _, errs) -> die (concat errs ++ usage) 1

process :: Config -> IO ()
process config = processFile (optPath config) (optFile config)

die :: String -> Int -> IO a
die s c = IO.hPutStr IO.stderr s >> X.exitWith (X.ExitFailure c)

--
-- Command line options
--

data Config = Config
    { optFile :: FilePath
    , optPath :: [FilePath]
    , optInteractive :: Bool
    } deriving (Show) 

defaultConfig :: Config
defaultConfig = Config
    { optFile = ""
    , optPath = [""]
    , optInteractive = False }

argspec :: [O.OptDescr (Config -> Config)]
argspec = 
    [ O.Option ['I'] ["includes"]    (O.OptArg addPath "DIR")  "search directory"
    , O.Option ['i'] ["interactive"] (O.NoArg setInteractive)  "interactive mode" ]

setFile :: FilePath -> Config -> Config
setFile fp c = c { optFile = fp }

setInteractive :: Config -> Config
setInteractive c = c { optInteractive = True }

addPath :: Maybe FilePath -> Config -> Config
addPath (Just fp) c = c { optPath = fp : optPath c }
addPath _ c = c

--
-- Process source file
--

processFile :: [FilePath] -> FilePath -> IO ()
processFile p f = do
    cmds <- parseFile p f emptyContext
    --mapM_ (print . processCommand) cmds
    -- (flip . flip foldM_) emptyContext cmds $ \ctx cmd -> do
    foldM_ processCommand emptyContext cmds
    IO.hFlush IO.stdout

parseFile :: [FilePath] -> FilePath -> Context -> IO [Command]
parseFile p f ctx =
    E.bracket (openFile p f) IO.hClose $ \h ->
        runParser untyped ctx f <$> S.hGetContents h >>= \r -> case r of
            Left e -> E.throwIO $ IO.userError (show e)
            Right ts -> return ts

openFile :: [FilePath] -> FilePath -> IO IO.Handle
openFile [] f = E.throwIO $ IO.userError ("Could not find: " ++ f)
openFile (p:ps) f =
    IO.openFile (name p f) IO.ReadMode `E.catch` \(e :: E.IOException) ->
        if IO.isDoesNotExistError e then openFile ps f else E.throwIO e
  where
    name "" f' = f'
    name p' f' = p' ++ "/" ++ f'

processPrompt :: IO ()
processPrompt = do
    putStrLn "        TaPL untyped version 0.0.0"
    putStrLn ""
    go emptyContext
  where
    go ctx = do
        putStr "# "
        IO.hFlush IO.stdout
        (cmds, ctx') <- runParser untypedForPrompt ctx "input" <$> S.getLine >>= \r -> case r of
            Left e -> putStrLn (show e) >> return ([], ctx)
            Right rs -> return rs
        foldM_ processCommand ctx cmds
        IO.hFlush IO.stdout
        go ctx'
    untypedForPrompt = do
        cmds <- untyped
        ctx <- getState
        return (cmds, ctx)

processCommand :: Context -> Command -> IO Context 
processCommand ctx (Eval term) = do
   --trace (show term) $ return ()
   printTerm ctx $ eval term
   putStrLn ""
   return ctx
processCommand ctx (Bind x bind) = do
   putStr x
   putStr " "
   printBinding bind 
   putStrLn ""
   return $ appendBinding ctx x bind

