module Main (main) where

-- create a few files with different number of lines taken
-- randomly from the input file

import Control.Applicative ((<$>))
import Control.Monad (forM_, foldM_)
-- import Data.Array.IArray
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
-- import Data.Ord (comparing)
import System.Console.GetOpt
-- import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Random

data Options = Options {
        optInFile :: Maybe String,	-- input file
        optRows   :: [Int]		-- list of no of rows in output files
    }

defaultOptions :: Options
defaultOptions = Options {
        optInFile = Nothing,
        optRows   = []
    }

setInFile :: String -> Options -> Options
setInFile cf opt = opt { optInFile = Just cf }

addLines :: String -> Options -> Options
addLines pa opt = opt { optRows = optRows opt ++ (map read $ splitOn "," pa) }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"] (ReqArg setInFile "STRING") "Intput file",
        Option "l" ["lines"] (ReqArg addLines  "STRING") "Lines for output files"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -i infile -l lines,lines,..."
          idName = "Divide"

main = do
    (opts, _) <- theOptions
    case optInFile opts of
        Nothing -> error "Input file must be given"
        Just fi -> if null (optRows opts)
                      then error "Rows for output files must be given"
                      else makeOutFiles fi (optRows opts)

makeOutFiles :: FilePath -> [Int] -> IO ()
makeOutFiles fi rs = do
    g <- getStdGen
    let rans = randoms g :: [Int]
    alfs <- map snd . sortBy (compare `on` fst) . zip rans . B.lines <$> B.readFile fi
    let (fne, ext) = splitExtension fi
    foldM_ (writeAnlFile fne ext) (1, cycle alfs) rs

writeAnlFile :: String -> String -> (Int, [B.ByteString]) -> Int -> IO (Int, [B.ByteString])
writeAnlFile fne ext (k, ls) n = do
    let (ls1, ls2) = splitAt n ls
        fok = addExtension (fne ++ "-" ++ show k) ext
    putStrLn $ "Write file " ++ fok ++ " with " ++ show n ++ " lines"
    h <- openFile fok WriteMode
    forM_ ls1 $ B.hPutStrLn h
    hClose h
    return (k+1, ls2)
