module Main (main) where

-- Analyse a FEN file with an UCI engine at a given depth
-- Produce an EPD file with fen & value on every line (comma separated)

import Control.Monad (when)
-- import Control.Exception
import Data.Traversable (forM)
import Data.List (isPrefixOf)
import Data.Time.Clock
import System.Console.GetOpt
import System.Directory (setCurrentDirectory, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath
import System.IO
-- import System.IO.Error
import System.Process
-- import Text.Printf

data Options = Options {
         optEngine :: String,
         optCwd    :: String,
         optDepth  :: Int
     } deriving Show

defaultOptions :: Options
defaultOptions = Options {
        optEngine = "",
        optCwd    = "",
        optDepth  = 8
    }

setEngdir :: String -> Options -> Options
setEngdir s opt = opt { optEngine = s }

setCwd :: String -> Options -> Options
setCwd s opt = opt { optCwd = s }

setDepth :: String -> Options -> Options
setDepth s opt = opt { optDepth = read s }

options :: [OptDescr (Options -> Options)]
options = [
        Option "e" ["engine"] (ReqArg setEngdir "STRING") "Engine file",
        Option "c" ["chdir"]  (ReqArg setCwd    "STRING") "Working directory",
        Option "d" ["depth"]  (ReqArg setDepth  "INTEGER") "Analyse depth"
    ]

usage :: String
usage = "Usage: AnalyseUci [-e engine] [-c chdir] [-d depth] input"

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo usage options))

main :: IO ()
main = do
    aopts <- theOptions
    -- putStrLn $ show aopts
    let (opts, rest) = aopts
    if length rest > 1
       then do
           putStrLn $ "Too many arguments: " ++ show rest
           ioError (userError (usageInfo usage options))
       else do
           setCurrentDirectory $ optCwd opts
           if length rest == 1
              then do
                  let input:_ = rest
                  oneFile opts input
              else allFilesInDir opts

oneFile :: Options -> String -> IO ()
oneFile opts input = do
    let output = replaceExtension input "epd"
    fex <- doesFileExist output
    if fex
       then do
           putStrLn $ "File " ++ output ++ " already exists, skip input " ++ input
           hFlush stdout
       else do
           putStrLn $ "Analyse with " ++ (optEngine opts) ++ " at depth " ++ show (optDepth opts)
           putStrLn $ "Input:  " ++ input ++ ", output: " ++ output
           hFlush stdout
           content <- readFile input
           withFile output WriteMode $ \hout -> oneProc opts content hout

allFilesInDir :: Options -> IO ()
allFilesInDir opts = do
    putStrLn $ "Analyse with " ++ optEngine opts ++ " at depth " ++ show (optDepth opts)
    putStrLn $ "All files in " ++ optCwd opts
    hFlush stdout
    fenfiles <- filter ((".fen" ==) . takeExtension) <$> listDirectory (optCwd opts)
    go fenfiles
    where
        go []           = return ()
        go (file:files) = do
           fex <- doesFileExist "stop"
           if fex
              then putStrLn "Stopped by existence of the stop file"
              else do
                  oneFile opts file
                  go files

oneProc :: Options -> String -> Handle -> IO ()
oneProc opts content houtfile = do
    (hin, hout, _, _ph) <- runInteractiveProcess (optEngine opts) [] (Just (optCwd opts)) Nothing
    hSetBuffering hin LineBuffering
    hPutStrLn hin "uci"
    _ <- lineUntil ("uciok" `isPrefixOf`) hout
    startTime <- getCurrentTime
    _ <- forM (zip [1::Int ..] (lines content)) $ \(i, fen) -> do
        hPutStrLn hin $ "position fen " ++ fen
        hPutStrLn hin $ "go depth " ++ show (optDepth opts)
        score <- accumLines hout ("bestmove " `isPrefixOf`) getScore 20000
        hPutStrLn houtfile $ fen ++ "," ++ show score
        -- putStrLn $ "Line " ++ show i ++ ": " ++ fen ++ " --> " ++ show score
        when (i `mod` 100000 == 0) $ do
            crtTime <- getCurrentTime
            let diff = nominalDiffTimeToSeconds $ diffUTCTime crtTime startTime
                lps  = (round $ fromIntegral i / diff) :: Int
            putStrLn $ show i ++ " fens, " ++ show lps ++ " fens per second"
            hFlush stdout
    hPutStrLn hin "quit"

-- errorHandling = do
--     r <- catch (runFen hin hout (optDepth opts)) $ \e -> do
--         let es = ioeGetErrorString e
--         putStrLn $ "Error in everyLine: " ++ es
--         terminateProcess ph
--         throwIO e
--     putStrLn $ engine ++ ": done, with " ++ show r

lineUntil :: (String -> Bool) -> Handle -> IO String
lineUntil p h = do
    l <- hGetLine h
    -- putStrLn l
    if p l then return l
           else lineUntil p h

accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             -- putStrLn l
             if p l then return a
                    else go $! f l a

getScore :: String -> Int -> Int
getScore l old
    | "info depth " `isPrefixOf` l = getSc l
    | otherwise = old

getSc :: String -> Int
getSc l
    | tp == "cp" = k
    | otherwise  = if k > 0 then 20000 - k else -20000 + k
    where ("score":tp:s:_) = dropWhile ((/=) "score") $ words l
          k = read s
