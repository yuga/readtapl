{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception     as E
import qualified Data.ByteString       as S
import qualified System.Console.GetOpt as O
import qualified System.Exit           as X
import qualified System.IO             as IO
import qualified System.IO.Error       as IO

import Control.Applicative
import Core
import Parser
import System.Environment    (getArgs, getProgName)
import Text.Parsec
import Debug.Trace

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
        (_, [],  []) -> die ("FILE is not specified\n" ++ usage) 1
        (o, f:_, []) -> process $ setFile f (foldl (.) id o defaultConfig)
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
    } deriving (Show) 

defaultConfig :: Config
defaultConfig = Config
    { optFile = ""
    , optPath = [""] }

argspec :: [O.OptDescr (Config -> Config)]
argspec = 
    [ O.Option ['I'] ["includes"] (O.OptArg addPath "DIR")  "search directory"]

setFile :: FilePath -> Config -> Config
setFile fp c = trace ("setFilei: fp=" ++ fp) $ c { optFile = fp }

addPath :: Maybe FilePath -> Config -> Config
addPath (Just fp) c = trace ("addPath: fp=" ++ fp) $ c { optPath = fp : optPath c }
addPath _ c = c

--
-- Process source file
--

processFile :: [FilePath] -> FilePath -> IO ()
processFile p f = do
    cmds <- E.bracket (openFile p f) IO.hClose $ \h ->
        parseFile f <$> S.hGetContents h >>= \r -> case r of
            Left e -> E.throwIO $ IO.userError (show e)
            Right ts -> return ts
    mapM_ (print . processCommand) cmds
    IO.hFlush IO.stdout

openFile :: [FilePath] -> FilePath -> IO IO.Handle
openFile [] f = E.throwIO $ IO.userError ("Could not find: " ++ f)
openFile (p:ps) f =
    IO.openFile (name p f) IO.ReadMode `E.catch` \(e :: E.IOException) ->
        if IO.isDoesNotExistError e then openFile ps f else E.throwIO e
  where
    name "" f' = f'
    name p' f' = p' ++ "/" ++ f'

parseFile :: FilePath -> S.ByteString -> Either ParseError [Command]
parseFile = runParser arith ()

processCommand :: Command -> Term
processCommand (Eval term) = eval term

