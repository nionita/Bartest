module Main (main) where

import Control.Exception
import Control.Monad (when)
import Data.List (isPrefixOf, intersperse)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

rin = "j:\\Barbarossa\\"
prf = "j:\\Barbarossa\\dist\\build\\Barbarossa\\"

data Options = Options {
        optEngine  :: String,		-- command string for engine
        optEngDir  :: FilePath,		-- where to run the engine
        optDepth   :: Maybe Int,	-- analyse depth (when creating the reference)
        optInpFile :: FilePath,		-- input file (test positions, reference results)
        optOutFile :: Maybe FilePath,	-- file (to create) with reference results
        optVerbose :: Bool		-- be explicit
    } deriving Show

defaultOptions :: Options
defaultOptions = Options {
        optEngine  = [],
        optEngDir  = [],
        optDepth   = Nothing,
        optInpFile = [],
        optOutFile = Nothing,
        optVerbose = False
    }

options :: [ OptDescr (Options -> Options)]
options = [
        Option "e" ["engine"] (ReqArg setEngine "STRING") "Engine command line",
        Option "p" ["path"]   (ReqArg setPath   "STRING") "Working directory for engine",
        Option "d" ["depth"]  (ReqArg setDepth  "INT")    "Analysis depth",
        Option "i" ["input"]  (ReqArg setInp    "STRING") "Input file with test positions",
        Option "o" ["output"] (ReqArg setOut    "STRING") "Output file (test pos & ref. scores)",
        Option "v" ["verbose"] (NoArg setVerbose)         "Verbose output"
    ]

theOptions :: IO Options
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, [], []) -> do
            let opt = foldr ($) defaultOptions o
            case inconsistentOptions opt of
                Nothing -> return opt
                Just ms -> ioError $ userError $ ms ++ usage
        (_, n, [])  -> ioError $ userError
                               $ "No extra argumens allowed (" ++ concat n ++ ")" ++ usage
        (_, _, es)  -> ioError $ userError $ concat es ++ usage
    where header = "\nUsage: -e engine -p dir -i infile [-d depth -o outfile]"
          usage  = usageInfo header options

inconsistentOptions :: Options -> Maybe String
inconsistentOptions opt = Nothing

setEngine :: String -> Options -> Options
setEngine cmd opt = opt { optEngine = cmd }

setPath :: String -> Options -> Options
setPath path opt = opt { optEngDir = path }

setDepth :: String -> Options -> Options
setDepth sd opt = opt { optDepth = Just $ read sd }

setInp :: String -> Options -> Options
setInp file opt = opt { optInpFile = file }

setOut :: String -> Options -> Options
setOut file opt = opt { optOutFile = Just file }

setVerbose :: Options -> Options
setVerbose opt = opt { optVerbose = True }

data Agreg = Agreg {
         agrCumErr :: !Integer,	-- accumulated error
         agrFenOk  :: !Int,	-- number of fens analysed
         agrFenNOk :: !Int	-- number of fens aborted
     } deriving Show

main = do
    opt <- theOptions
    case optOutFile opt of
        Just outf -> referenceRun opt
        Nothing   -> testRun opt

testRun :: Options -> IO ()
testRun opt = do
    inp <- readFile $ optInpFile opt
    let header : fens = lines inp
        "Reference" : "depth" : depth : refeng = words header
        prog : args = words $ optEngine opt
        verb = optVerbose opt
    (hin, hout, _, ph) <- runInteractiveProcess prog args (Just $ optEngDir opt) Nothing
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering
    setUp verb hin hout 64
    agr <- foldrM (perFenLineEng verb hin hout depth) (Agreg 0 0 0) fens
    hPutStrLn hin "quit"
    putStrLn $ show agr

referenceRun :: Options -> IO ()
referenceRun opt = do
    inp  <- readFile $ optInpFile opt
    filo <- openFile (fromJust $ optOutFile opt) WriteMode
    let sdepth = show (fromJust $ optDepth opt) -- we need it as string
        cmd = optEngine opt
        prog : args = words cmd
        verb = optVerbose opt
    putStrLn $ "Running reference: depth " ++ sdepth ++ " engine " ++ cmd
    hPutStrLn filo $ "Reference depth " ++ sdepth ++ " Engine " ++ cmd
    (hin, hout, _, ph) <- runInteractiveProcess prog args (Just $ optEngDir opt) Nothing
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering
    setUp verb hin hout 64
    mapM_ (perFenLineRef verb hin hout filo sdepth) $ map correctFen $ lines inp
    hPutStrLn hin "quit"
    hClose filo

-- Setup the engine
setUp :: Bool -> Handle -> Handle -> Int -> IO ()
setUp verb hi ho mb = do
    hPutStrLn hi "uci"
    lineUntil verb ho ("uciok" `isPrefixOf`)
    hPutStrLn hi $ "setoption name Hash value " ++ show mb
    hPutStrLn hi "isready"
    lineUntil verb ho ("readyok" `isPrefixOf`)
    return ()

-- Execute for every Fen line
perFenLineEng :: Bool -> Handle -> Handle -> String -> String -> Agreg -> IO Agreg
perFenLineEng verb hi ho depth fenLine agr = do
    -- putStrLn $ "Fen line: " ++ fenLine
    let (refsc, fen') = break ((==) '\t') fenLine
        rsc = read refsc
        fen = tail fen'	-- it has the \t in front
    when verb $ putStrLn $ "Refsc = " ++ show rsc ++ ", fen: " ++ fen
    hPutStrLn hi $ "position fen " ++ fen
    hPutStrLn hi $ "go depth " ++ depth
    msc <- accumLines verb ho endOfFen getBestScore Nothing
    case msc of
        Just sc -> return $ aggregateError agr rsc sc
        Nothing -> return $ agr { agrFenNOk = agrFenNOk agr + 1 }

endOfFen :: String -> Bool
endOfFen l = "bestmove" `isPrefixOf` l || l == "info string empty pv"

-- Gather the last (best) score from engine output
getBestScore :: String -> Maybe Int -> Maybe Int
getBestScore line osc
    | "info" `isPrefixOf` line = scoreFromInfo line osc
    | otherwise                = osc

scoreFromInfo :: String -> Maybe Int -> Maybe Int
scoreFromInfo line osc
    | "score" : sctype : scval : _ <- dropWhile (/= "score") (words line)
                = if sctype == "cp"
                     then Just $ read scval
                     else let s:_ = scval	-- here is mated
                          in if s == '-' then Just (-20000) else Just 20000
    | otherwise = osc

aggregateError :: Agreg -> Int -> Int -> Agreg
aggregateError agr refsc sc
    = agr { agrCumErr = agrCumErr agr + fromIntegral (dif * dif), agrFenOk = agrFenOk agr + 1 }
    where dif = sc - refsc

-- Execute for every Fen line for reference engine
perFenLineRef :: Bool -> Handle -> Handle -> Handle -> String -> String -> IO ()
perFenLineRef verb hi ho fo depth fen = do
    hPutStrLn hi $ "position fen " ++ fen
    hPutStrLn hi $ "go depth " ++ depth
    -- putStrLn $ "Fen: " ++ fen
    msc <- accumLines verb ho endOfFen getBestScore Nothing
    case msc of
        Just sc -> hPutStrLn fo $ show sc ++ "\t" ++ fen
        Nothing -> putStrLn $ "NONE" ++ "\t" ++ fen
    -- threadDelay 3000	-- wait a bit in hope the engine will have time to breath

-- Read lines until one condition occurs
lineUntil :: Bool -> Handle -> (String -> Bool) -> IO String
lineUntil verb h p = go
    where go = do l <- hGetLine h
                  when verb $ putStrLn $ "lU: " ++ l
                  if p l then return l
                         else go

-- Read lines until one condition occurs, accumulating some
-- information from the read lines
accumLines :: Bool -> Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines verb h p f = go
    where go a = do
             l <- hGetLine h
             when verb $ putStrLn $ "aL: " ++ l
             if p l then return a
                    else go $! f l a

-- Theese are here just in case we want more info (for statistics)
getTimeNodes :: String -> (Int, Int) -> (Int, Int)
getTimeNodes l old
    | "info score " `isPrefixOf` l = getTN l
    | otherwise = old

getTN :: String -> (Int, Int)
getTN l = (read t, read n)
    where ws = words l
          ("time":t:rest) = dropWhile (/= "time") ws
          ("nodes":n:_) = dropWhile (/= "nodes") rest

correctFen :: String -> String
correctFen fen
    | fig : col : cast : ep : hm : nr : _ <- words fen
        = concat $ intersperse " " [fig, col, correct cast, ep, hm, nr]
    where correct "----" = "-"	-- this has to be continued
          correct x      = x	-- see http://de.wikipedia.org/wiki/Forsyth-Edwards-Notation
