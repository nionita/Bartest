module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad (when, forever)
import Data.Foldable (forM_)
import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Process

{-
 - Filter the input fen file by analysing every fen with the given UCI engine to a given depth
 - Only fens which are within a score of +- the given score (in centi pawns) will be
 - copied to the output file (also fen file)
 - I.e. we keep almost balanced positions
 -}

data Options = Options {
         optInFile  :: String,
         optOutFile :: String,
         optEngdir  :: String,
         optCwd     :: String,
         optThreads :: Int,
         optMaxFens :: Int,
         optDepth   :: Int,
         optLow     :: Int,
         optHigh    :: Int
     }

defaultOptions :: Options
defaultOptions = Options {
        optInFile  = "onePawn.fen",
        optOutFile = "result.fen",
        optEngdir  = "C:\\Engines\\Barbarossa\\",
        optCwd     = "C:\\Engines\\Barbarossa\\",
        optThreads = 2,
        optMaxFens = 10,
        optDepth   = 10,
        optLow     = 75,
        optHigh    = 150
    }

setInFile :: String -> Options -> Options
setInFile s opt = opt { optInFile = s }

setOutFile :: String -> Options -> Options
setOutFile s opt = opt { optOutFile = s }

setEngdir :: String -> Options -> Options
setEngdir s opt = opt { optEngdir = s }

setCwd :: String -> Options -> Options
setCwd s opt = opt { optCwd = s }

setDepth :: String -> Options -> Options
setDepth s opt = opt { optDepth = read s }

setThreads :: String -> Options -> Options
setThreads s opt = opt { optThreads = read s }

setMaxFens :: String -> Options -> Options
setMaxFens s opt = opt { optMaxFens = read s }

setLow :: String -> Options -> Options
setLow s opt = opt { optLow = read s }

setHigh :: String -> Options -> Options
setHigh s opt = opt { optHigh = read s }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]   (ReqArg setInFile  "STRING")  "Input file",
        Option "o" ["output"]  (ReqArg setOutFile "STRING")  "Output file",
        Option "e" ["engdir"]  (ReqArg setEngdir  "STRING")  "Engine directory",
        Option "c" ["chdir"]   (ReqArg setCwd     "STRING")  "Working directory",
        Option "d" ["depth"]   (ReqArg setDepth   "INTEGER") "Analyse depth",
        Option "t" ["threads"] (ReqArg setThreads "INTEGER") "Number of threads",
        Option "m" ["maxfens"] (ReqArg setMaxFens "INTEGER") "Maximum number of fens",
        Option "l" ["low"]     (ReqArg setLow     "INTEGER") "Low score",
        Option "h" ["high"]    (ReqArg setHigh    "INTEGER") "High score"
    ]

header :: String
header = "Usage: FilterFen [-i infile] [-o outfile] [-e engdir] [-c chdir] [-d depth] [-l low] [-h high] engine"

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))

main :: IO ()
main = do
    (opts, engine:_) <- theOptions
    mvar <- startAndWrite opts engine
    -- startAndWrite is starting writer & worker threads
    -- then writes all fens to the worker channels
    -- At the end it signals them that we are done, so they end themselves
    -- and communicate via MVar this status, which is awaited for in startAndWrite
    -- Then startAndWrite is done, the writer channel has no other end connected to it
    -- and the writer gets an exception BlockedIndefinitelyOnMVar when reading from his channel
    -- So it aborts and writes in the MVar, for which the main thread is waiting
    takeMVar mvar

startAndWrite :: Options -> String -> IO (MVar ())
startAndWrite opts engine = do
    (cho, mvar) <- startWriter (optOutFile opts)
    chmvs <- sequence $ take (optThreads opts) $ repeat $ startWorker opts engine cho
    let (chis, mvars) = unzip chmvs
    inp <- readFile $ optInFile opts
    let todo | k <- optMaxFens opts = take k (lines inp)
             | otherwise            = lines inp
    forM_ (zip (cycle chis) $ zip [1..] todo) $ \(chi, (i, line)) -> do
        when (i `mod` 1000 == (0 :: Int)) $ putStrLn $ "Added " ++ show i ++ " fens"
        writeChan chi $ Just line
    -- Signal worker threads that we are done
    putStrLn "Signal: End of work"
    forM_ chis $ \chi -> writeChan chi Nothing
    -- Wait for all mvars of the workers
    putStrLn "Waiting for the workers"
    forM_ mvars $ \mv -> takeMVar mv
    return mvar

startWriter :: String -> IO (Chan String, MVar ())
startWriter ofile = do
    h <- openFile ofile AppendMode
    cho <- newChan
    mvar <- newEmptyMVar
    _ <- forkIO $ theWriter cho h `finally` (do
                hClose h
                putMVar mvar ()
         )
    putStrLn $ "Writer started for " ++ ofile
    return (cho, mvar)

theWriter :: Chan String -> Handle -> IO ()
theWriter ch h = forever $ do
    fen <- readChan ch
    hPutStrLn h fen

startWorker :: Options -> String -> Chan String -> IO (Chan (Maybe String), MVar ())
startWorker opts engine cho = do
    chi <- newChan
    mvar <- newEmptyMVar
    _ <- forkFinally (oneThread opts engine chi cho) $ \_ -> do
        putStrLn "Worker ended"
        putMVar mvar ()
    return (chi, mvar)

-- This runs a worker thread, catching exceptions and restarting when failure
oneThread :: Options -> String -> Chan (Maybe String) -> Chan String -> IO ()
oneThread opts engine chi cho = do
    (hin, hout, _, ph)
         <- runInteractiveProcess (optEngdir opts ++ engine) [] (Just (optCwd opts)) Nothing
    hSetBuffering hin LineBuffering
    catch (runWorker chi cho hin hout (optLow opts) (optHigh opts) (optDepth opts)) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in worker thread: " ++ es
        terminateProcess ph
        oneThread opts engine chi cho

-- This runs an engine communication, passing new fens and collecting the score
runWorker :: Chan (Maybe String) -- input fen channel
          -> Chan String -- output fen channel
          -> Handle      -- handle to write to the engine
          -> Handle      -- handle to read from the engine
          -> Int         -- minimum score
          -> Int         -- maximum score
          -> Int         -- depth
          -> IO ()
runWorker chi cho hi ho low high depth = do
    hPutStrLn hi "uci"
    _ <- lineUntil ho ("uciok" `isPrefixOf`)
    perFen chi cho hi ho low high depth

perFen :: Chan (Maybe String) -- input fen channel
       -> Chan String -- output fen channel
       -> Handle      -- handle to write to the engine
       -> Handle      -- handle to read from the engine
       -> Int         -- minimum score
       -> Int         -- maximum score
       -> Int         -- depth
       -> IO ()
perFen chi cho hi ho low high depth = do
    mfen <- readChan chi
    case mfen of
        Nothing -> return ()
        Just fen -> do
            hPutStrLn hi $ "position fen " ++ fen
            hPutStrLn hi $ "go depth " ++ show depth
            s <- accumLines ho ("bestmove " `isPrefixOf`) getScore Nothing
            case s of
                Just sc -> do
                    let asc = abs sc
                    when (asc >= low && asc <= high) $ writeChan cho fen
                Nothing -> return ()
            perFen chi cho hi ho low high depth

lineUntil :: Handle -> (String -> Bool) -> IO String
lineUntil h p = do
    l <- hGetLine h
    -- putStrLn l
    if p l then return l
           else lineUntil h p

accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             -- putStrLn l
             if p l then return a
                    else go $! f l a

getScore :: String -> Maybe Int -> Maybe Int
getScore l old
    | "info score " `isPrefixOf` l, (_:_:_:ss:_) <- words l = Just (read ss)
    | otherwise                                             = old
