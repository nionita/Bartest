module Main (main) where

import Control.Concurrent.Async
import Control.Exception
import Data.Foldable (foldlM)
import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import Text.Printf

data Options = Options {
         optEngdir :: String,
         optCwd    :: String,
         optDepth  :: Int,
         optRept   :: Int
     }

defaultOptions :: Options
defaultOptions = Options {
        optEngdir = "j:\\Engines\\Barbarossa\\",
        optCwd    = "j:\\Engines\\Barbarossa\\",
        optDepth  = 12,
        optRept   = 1
    }

setEngdir :: String -> Options -> Options
setEngdir s opt = opt { optEngdir = s }

setCwd :: String -> Options -> Options
setCwd s opt = opt { optCwd = s }

setDepth :: String -> Options -> Options
setDepth s opt = opt { optDepth = read s }

setRept :: String -> Options -> Options
setRept s opt = opt { optRept = read s }

options :: [OptDescr (Options -> Options)]
options = [
        Option "e" ["engdir"] (ReqArg setEngdir "STRING") "Engine directory",
        Option "c" ["chdir"]  (ReqArg setCwd    "STRING") "Working directory",
        Option "d" ["depth"]  (ReqArg setDepth  "INTEGER") "Analyse depth",
        Option "r" ["repeat"] (ReqArg setRept   "INTEGER") "Repetitions"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: CompUci [-e engdir] [-c chdir] [-d depth] [-r n] engine1 engine2"

main = do
    (opts, engine1:engine2:_) <- theOptions
    putStrLn "Engines:"
    putStrLn $ "  " ++ engine1
    putStrLn $ "  " ++ engine2
    putStrLn $ "Analyse " ++ show (optRept opts) ++ " times at depth " ++ show (optDepth opts)
    ((t1, n1), (t2, n2)) <- foldlM (\((tt1, tn1), (tt2, tn2)) i -> do
        putStrLn $ "Iteration " ++ show i ++ " of " ++ show (optRept opts)
        r1 <- async $ oneProc opts engine1
        r2 <- async $ oneProc opts engine2
        (t1, n1) <- wait r1
        (t2, n2) <- wait r2
        return ((tt1+t1, tn1+n1), (tt2+t2, tn2+n2))
        ) ((0,0), (0,0)) [1..(optRept opts)]
    printRes engine1 n1 t1
    printRes engine2 n2 t2

printRes :: String -> Int -> Int -> IO ()
printRes eng n t = do
    putStrLn $ printf "%-29s %11d nodes, %8d ms: %6d nodes/sec" (eng ++ ":") n t r
    where r = round v :: Int
          v = fromIntegral n * 1000 / fromIntegral t :: Double

oneProc :: Options -> String -> IO (Int, Int)
oneProc opts engine = do
    (hin, hout, _, ph)
         <- runInteractiveProcess (optEngdir opts ++ engine) [] (Just (optCwd opts)) Nothing
    hSetBuffering hin LineBuffering
    r <- catch (runFen hin hout (optDepth opts)) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        throwIO e
    putStrLn $ engine ++ ": done, with " ++ show r
    return r

lineUntil :: (String -> Bool) -> Handle -> IO String
lineUntil p h = do
    l <- hGetLine h
    -- putStrLn l
    if p l then return l
           else lineUntil p h

-- depth = 12	-- fix depth
-- ttime = 480000

runFen :: Handle -> Handle -> Int -> IO (Int, Int)
runFen hi ho depth = do
    hPutStrLn hi "uci"
    lineUntil ("uciok" `isPrefixOf`) ho
    hPutStrLn hi $ "position startpos"	-- evtl with fen
    hPutStrLn hi $ "go depth " ++ show depth
    -- hPutStrLn hi $ "go movestogo 1 wtime " ++ show ttime
    (t, n) <- accumLines ho ("bestmove " `isPrefixOf`) getTimeNodes (1, 0)
    hPutStrLn hi "quit"
    return (t, n)

accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             -- putStrLn l
             if p l then return a
                    else go $! f l a

getTimeNodes :: String -> (Int, Int) -> (Int, Int)
getTimeNodes l old
    | "info score " `isPrefixOf` l = getTN l
    | otherwise = old

getTN :: String -> (Int, Int)
getTN l = (read t, read n)
    where ws = words l
          ("time":t:rest) = dropWhile (/= "time") ws
          ("nodes":n:_) = dropWhile (/= "nodes") rest
