module Main (main) where

import Control.Exception
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

rin = "j:\\Barbarossa\\"
prf = "j:\\Barbarossa\\dist\\build\\Barbarossa\\"

main = do
    (engine:_) <- getArgs
    oneProc engine

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
    -- terminateProcess ph	-- do we need this?
    return r

lineUntil :: (String -> Bool) -> Handle -> IO String
lineUntil p h = do
    l <- hGetLine h
    putStrLn l
    if p l then return l
           else lineUntil p h

depth = 12	-- fix depth
ttime = 1000000

runFen :: Handle -> Handle -> IO (Int, Int)
runFen hi ho = do
    hPutStrLn hi "uci"
    lineUntil ("uciok" `isPrefixOf`) ho
    hPutStrLn hi $ "position startpos"	-- evtl with fen
    -- hPutStrLn hi $ "go depth " ++ show depth
    hPutStrLn hi $ "go wtime " ++ show ttime
    (t, n) <- accumLines ho ("bestmove " `isPrefixOf`) getTimeNodes (1, 0)
    hPutStrLn hi "quit"
    return (t, n)

accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             putStrLn l
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
