module Main (main) where

-- CLOP works good when there is a unique maximum in the parameter space
-- But sometimes this is not the case, especially when the parameter domain
-- is large
-- If, after a while running Clop, we have a reason to think that there are
-- at least 2 maximum points (of course local), then we may whish to
-- restrict the parameter domain, so that Clop can converge faster to a unique
-- maximum
--
-- But we also want to reuse the already played games, i.e. create a
-- Clop .dat file containing only games with parameters in the new domain
-- and continue wtih the new domain from that point
--
-- This is what this program does
--
-- Currently (version 0.09) Clop's dat file has following format:
-- One record per line, plain text
-- A record has a type and 1 to 3 parameters, depending on record type
-- Record types:
-- N -> dimension, e.g.: N 2
-- P -> parameter name, e.g.: P mid.isolPassed
-- S -> game start, with game number & param values, e.g.: S 7 -122 35
-- R -> game result, with game number and result, e.g.: R 7 0
--
-- We pass through all N and P records and S or R records will be filtered out
-- S based on relation of the parameter point to parameter domain,
-- and R based on the decision made for the corresponding S game number
-- While we filter S/R records out, we rename the games, not to have holes

import qualified Data.IntMap.Lazy as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

data Options = Options {
        optInFile  :: Maybe String,  -- input file
        optOutFile :: Maybe String,  -- output file
        optDir     :: Maybe String   -- change directory
    }

defaultOptions :: Options
defaultOptions = Options {
        optInFile  = Nothing,
        optOutFile = Nothing,
        optDir     = Nothing
    }

setInFile :: String -> Options -> Options
setInFile cf opt = opt { optInFile = Just cf }

setOutFile :: String -> Options -> Options
setOutFile cf opt = opt { optOutFile = Just cf }

setDir :: String -> Options -> Options
setDir cf opt = opt { optDir = Just cf }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]  (ReqArg setInFile  "STRING") "Input file",
        Option "o" ["output"] (ReqArg setOutFile "STRING") "Output file",
        Option "d" ["dir"]    (ReqArg setDir     "STRING") "Change working directory"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: ReClop [-d dir] -i infile -o outfile min:max ..."

main :: IO ()
main = do
    (opts, rest) <- theOptions
    case optInFile opts of
        Nothing -> error "Input file must be given"
        Just fi -> case optOutFile opts of
                       Nothing -> error "Output file must be given"
                       Just fo -> do
                           case optDir opts of
                               Nothing -> return ()
                               Just d  -> setCurrentDirectory d
                           hi <- openFile fi ReadMode
                           ho <- openFile fo WriteMode
                           loop hi ho (intervals rest) Nothing

loop :: Handle -> Handle -> [(Double, Double)] -> Maybe State -> IO ()
loop hi ho is ms = do
    eof <- hIsEOF hi
    if eof
       then return ()  -- should check that no game remained
       else do
           line <- T.hGetLine hi
           let (w:ws) = T.words line
               rec | w == T.pack "S", (sg:sps)   <- ws
                       = S (read $ T.unpack sg) $ map (read . T.unpack) sps
                   | w == T.pack "R", (sg:sr:[]) <- ws = R (read $ T.unpack sg) sr
                   | w == T.pack "P", (sp:[])    <- ws = P sp
                   | w == T.pack "N", (sd:[])    <- ws = N sd
                   | otherwise                  = error $ "Parse line: " ++ T.unpack line
               (ms', ors) = nextState is rec ms
           mapM_ (T.hPutStrLn ho . showRecord) ors
           loop hi ho is ms'

intervals :: [String] -> [(Double, Double)]
intervals = map interval

interval :: String -> (Double, Double)
interval cs = (read mn, read mx)
    where ('P':mn, ':':mx) = break ((==) ':') cs

newtype Game = Game [Double]  -- just a rename list of param values

data State = State { stGameNo :: !Int, stGames :: M.IntMap Game }

data Record = N !T.Text
            | P !T.Text
            | S !Int [Double]
            | R !Int !T.Text

showRecord :: Record -> T.Text
showRecord (N t) = T.pack "N " `T.append` t
showRecord (P t) = T.pack "P " `T.append` t
showRecord (S g ds) = T.intercalate (T.pack " ")
                          $ [T.pack "S", T.pack (show g) ] ++ map (T.pack . show) ds
showRecord (R g t) = T.pack "R " `T.append` (T.pack $ show g) `T.append` (T.pack " ") `T.append` t

-- This is the transition function, taking an input:
-- - list of parameter intervals
-- - an input record and
-- - maybe a state
-- and returning maybe a state and a list of output records
nextState :: [(Double, Double)] -> Record -> Maybe State -> (Maybe State, [Record])
nextState _  (N _)    (Just _) = error "Unexpected dimension record found"
nextState _  (N n)     Nothing = (Just State { stGameNo = 0, stGames = M.empty }, [N n])
nextState _  (P t)    (Just s) = (Just s, [P t])
nextState is (S g ds) (Just s)
    | ok is ds  = (Just s { stGames = M.insert g (Game ds) (stGames s) }, [])
    | otherwise = (Just s, [])
nextState _  (R g r)  (Just s@(State { stGameNo = n, stGames = gset }))
    | Just (Game ds) <- M.lookup g gset
        = let gset' = M.delete g gset
              n'    = n + 1
          in (Just s { stGameNo = n', stGames = gset' }, [S n ds, R n r])
    | otherwise = (Just s, [])
nextState _  _      Nothing = error "First file line must be dimension record"

ok :: [(Double, Double)] -> [Double] -> Bool
ok is ds = and $ zipWith between is ds

between :: (Double, Double) -> Double -> Bool
between (a, b) c = a <= c && c <= b
