module Main (main) where

import Control.Concurrent.Async
import Control.Exception
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

rin = "j:\\Engines\\Barbarossa\\"
prf = "j:\\Engines\\Barbarossa\\"

main = do
    (engine1:engine2:_) <- getArgs
    r1 <- async $ oneProc engine1
    r2 <- async $ oneProc engine2
    (t1, n1) <- wait r1
    (t2, n2) <- wait r2
    printRes engine1 n1 t1
    printRes engine2 n2 t2

printRes eng n t = do
    putStrLn $ "Engine " ++ eng ++ ": " ++ show n ++ " nodes, " ++ show t ++ " ms: "
                 ++ show (round v) ++ " nodes/sec"
    where v :: Double
          v = fromIntegral n * 1000 / fromIntegral t

oneProc :: String -> IO (Int, Int)
oneProc engine = do
    (hin, hout, _, ph)
         <- runInteractiveProcess (prf ++ engine) [] (Just rin) Nothing
    hSetBuffering hin LineBuffering
    r <- catch (runFen hin hout) $ \e -> do
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

depth = 12	-- fix depth
ttime = 480000

runFen :: Handle -> Handle -> IO (Int, Int)
runFen hi ho = do
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
