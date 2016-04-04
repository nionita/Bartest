{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- This is a program to optimise chess engine parameters by running tournaments with cutechess-cli
-- Optimisation method is bayesian optimisation using bayesopt library (global, stochastic optimisation)
-- The optimsation is driven by a configuration file which specifies:
-- - learning engine to use as an optimisation subject (i.e. receives parameter name, value pairs
--   and is running accordingly, the results of optimisations are the parameters which maximises
--   the performance of that engine)
-- - reference engines (play gauntlet of challenger against all reference engines)
-- - cutechess-cli configuration (times/depths, positions database, draw/adjudication conditions etc)
-- - parameters to be optimised (name, range)

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.IORef
import           Data.List (isPrefixOf)
import           Data.Ratio
import           Data.Serialize (Serialize, encodeLazy, decodeLazy)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import           Data.Yaml
import           GHC.Generics
-- import           Optimisation.Stochastic.BayesOpt
import           BayesOpt
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Process
-- import           System.Timeout
-- import           System.Time (getClockTime)

-- We have to use the first parameter from CLOP twofold:
-- first part will be a subdirectory in which the CLOP learn session will run
-- second part will a unique identifier for the parallel process
-- The 2 parts are delimitted by an underscore "_"
main :: IO ()
main = do
    (rootdir : _) <- getArgs
    prepareGames $ normalise rootdir

-- This data has to be passed to our callback, so it can do its work
-- But we must change it during the callback, so we will pass a reference to it
data RunState = RunState {
                    rsCCC    :: FilePath,     -- how to start cutechess-cli
                    rsEvent  :: String,       -- event name used for file names
                    rsRoot   :: FilePath,     -- root directory, used to start there
                    rsParams :: [String],     -- param list needed to write engine config
                    rsArgs   :: [String],     -- arguments for cutechess-cli
                    rsLast   :: Int,          -- last run number
                    rsChaEng :: String,       -- the learning engine name
                    rsMaxPts :: Double,       -- max number of points
                    rsRepls  :: [Double],     -- to use for replays
                    rsVTPla  :: Int,          -- number of simple tournaments played
                    rsVSX    :: Double,       -- sum of simple results
                    rsVSX2   :: Double        -- sum of quadrats of simple results
                }
    deriving Show

-- Parts of RunState which are saved & restored in case of interrupts
data Save = Save {
                saLast  :: Int,     -- last run number
                saVTPla :: Int,     -- number of simple tournaments played
                saVSX   :: Double,  -- sum of simple results
                saVSX2  :: Double   -- sum of quadrats of simple results
            } deriving Generic

instance Serialize Save

confFile, runFile, saveFile :: FilePath
confFile = "boconfig.cfg"
runFile  = "running"
saveFile = "rssave.dat"

-- This will prepare the begin of the match and return a run state
-- which can be used to begin the optimisation
-- Care must be taken with save/restore files, as we will need to save/restore
-- the last played game, otherwise we will overwrite logs & pgns
-- The given file path is supposed to be the root directory of the experiment.
-- This will also be the directory in which cutechess-cli will run
-- Under this root directory we will have this structure of files/subdirectories:
-- - the optimisation configuration file, called boconfig.cfg
-- - a cutechess-cli config file, named engines.json, with the engine descriptions
-- - a pgn output directory, called pgnout, in which every tour run writes one pgn file
-- - a log directory, called logdir, where per tour some log infos will be written
-- - per engine one cwd directory, which eventually further files if needed
-- - in the learning engines directory one config file will be written for every tour
--   run, which contain the parameters for that run
-- While the optimisation runs, there will be a running file (empty), which will
-- be deleted on complition.
prepareGames :: FilePath -> IO ()
prepareGames rootdir = do
    setCurrentDirectory rootdir
    cfe <- doesFileExist confFile
    when (not cfe) $ fail $ "Config file " ++ confFile ++ " does not exist in the given directory"
    mconf <- decodeFileEither confFile
    case mconf of
        Left ye      -> fail $ "Existing config file does not parse: " ++ show ye
        Right config -> do
            -- create directories
            createDirectoryIfMissing False "logdir"
            createDirectoryIfMissing False "pgnout"
            eco <- openFile "engines.json" WriteMode
            hPutStrLn eco "["
            engines <- forM (zip (False:repeat True)
                                 (coChaEng config : coRefEngs config)) $ \(komma, e) -> do
                let ename = T.unpack (engName e)
                let edir = "cwd-" ++ ename
                createDirectoryIfMissing False edir
                when (komma) $ hPutStrLn eco $ tab ++ ","
                hPutStrLn eco $ tab ++ "{"
                hPutStrLn eco $ tab ++ tab ++ qot ++ "name" ++ qot ++ " : "
                                ++ qot ++ ename ++ qot ++ ","
                hPutStrLn eco $ tab ++ tab ++ qot ++ "command" ++ qot ++ " : "
                                ++ qot ++ engBin e ++ qot ++ ","
                hPutStrLn eco $ tab ++ tab ++ qot ++ "workingDirectory" ++ qot ++ " : "
                                -- </> makes problems in Windows:
                                ++ show (normalise (rootdir </> edir)) ++ ","
                putStrLn $ show (normalise (rootdir </> edir))
                hPutStrLn eco $ tab ++ tab ++ qot ++ "protocol" ++ qot ++ " : "
                                ++ qot ++ T.unpack (engProto e) ++ qot
                hPutStrLn eco $ tab ++ "}"
                return ename
            hPutStrLn eco "]"
            hClose eco
            let rep = coReplay config
                replays = reStart rep : map (nextReplay (reRest rep)) replays
                rs' = RunState {
                         rsCCC    = coCCC config,
                         rsEvent  = T.unpack (toEvent $ coTour config),
                         rsRoot   = rootdir,
                         rsParams = map (T.unpack . opParam) $ coParams config,
                         rsArgs   = mkCutechessCommand config,
                         rsLast   = 0,
                         rsChaEng = head engines,
                         rsMaxPts = fromIntegral $ toRounds (coTour config) * 2
                                        * (length engines - 1),
                         rsRepls  = replays,
                         rsVTPla  = 0,
                         rsVSX    = 0,
                         rsVSX2   = 0
                     }
            -- putStrLn "A few replays:"
            -- print $ take 6 replays
            rs <- loadStatus rs'
            let vl = V.fromList $ map opLower $ coParams config
                vu = V.fromList $ map opUpper $ coParams config
            rrs <- newIORef rs
            -- Write an empty running file (when deleted: stop)
            writeFile runFile ""
            -- Start the (long) optimisation:
            (r, vr) <- bayesOptim runGame rrs vl vu (coIters config) (coNoise config)
            putStrLn $ "minimus is: " ++ show r
            putStrLn "for following parameters:"
            forM_ (zip (rsParams rs) (V.toList vr))
                $ \(pn, val) -> putStrLn $ " - " ++ pn ++ " = " ++ show val
    where tab = "\t"
          qot = "\""

-- We play small tournaments by default, to keep the run time shorter
-- But this brings high noise in the results, so we take following approach:
-- If the result is poor, we take it as it is
-- but if it is good, we increase the number of games, to see if it was just noise
-- or a really good point - the higher, the more games
-- This is the function to calculate the fractions of points from which we need
-- more and more replays
nextReplay :: Double -> Double -> Double
nextReplay frac lst = lst + frac * (1 - lst)

-- This is the callback of the optimisation process, the function
-- which will be called to acquire the target value for some parameter values
-- Prepare the environment (i.e. config file, log files, pgn file names)
-- and then start cutechess-cli for a tour, waiting for results
-- The leaning engine must understand command line option -c config, where in config
-- the parameters are written one per line in form:
-- name=value
-- Other command line options can be given in the global config file (for all engines)
runGame :: Callback (IORef RunState)
runGame rsref v = do
    -- tod <- getClockTime
    rs <- readIORef rsref
    let crtrun  = rsLast rs + 1
        runname = rsEvent rs ++ "-run-" ++ show crtrun
        logfile = normalise (rsRoot rs </> "logdir" </> (runname ++ ".log"))
        pgnout  = normalise (rsRoot rs </> "pgnout" </> (runname ++ ".pgn"))
        engcfg  = normalise (rsRoot rs </> ("cwd-" ++ rsChaEng rs) </> (runname ++ ".cfg"))
    -- write a new log file:
    --     ++ (calendarTimeToString $ toCalendarTime tod) ++ "\n"
    -- write engine config file:
    writeFile engcfg $ unlines $ map toEngConfLine $ zip (rsParams rs) $ V.toList v
    let args = addCutechessPgnOut (rsChaEng rs) engcfg pgnout (rsArgs rs)
    writeFile logfile $ unlines [
        "New run " ++ runname ++ " started",
        "Start: " ++ show (rsCCC rs),
        "with args: " ++ show args
        ]
    (prr, srs) <- plausibleRun (rsRepls rs) (rsMaxPts rs) (rsCCC rs) args (rsChaEng rs)
    let rs' = rs { rsLast  = crtrun,
                   rsVTPla = rsVTPla rs + length srs,
                   rsVSX   = rsVSX   rs + sum srs,
                   rsVSX2  = rsVSX2  rs + sum (map sqr srs)
                 }
        n   = fromIntegral $ rsVTPla rs'
        msr = rsVSX rs' / n
        var = rsVSX2 rs' / n - sqr msr
    putStrLn "Results statistics:"
    putStrLn $ "Number of tournaments played: " ++ show (rsVTPla rs')
    putStrLn $ "Mean of simple results:       " ++ show msr
    putStrLn $ "Signal + Noise variance:      " ++ show var
    writeIORef rsref rs'
    saveStatus rs'
    -- return difference to 1, so that minimize will maximize the performance
    return $! 1 - prr
    where sqr x = x * x

plausibleRun :: [Double] -> Double -> FilePath -> [String] -> String -> IO (Double, [Double])
plausibleRun replays maxpts ccc args me = go 0 0 replays []
    where go pts0 played (pr:prs) srs = do
              putStrLn $ "Play new tournament for perf " ++ show pr
              pts1 <- fromRational <$> oneMatch ccc args me
              let pts  = pts0 + pts1
                  sr   = pts1 / maxpts  -- simple result of last match
                  srs' = sr : srs
                  mxp  = maxpts * (played + 1)
                  prf  = pts / mxp
              if prf <= pr
                 then return (prf, srs') -- we played enough
                 else do
                     putStrLn $ "Perf is " ++ show prf
                     go pts (played + 1) prs srs'
          go _ _ [] _ = fail "Infinite sequence is empty??"

-- Save the status to file
saveStatus :: RunState -> IO ()
saveStatus rs = do
    let ss = Save { saLast = rsLast rs, saVTPla = rsVTPla rs,
                    saVSX  = rsVSX  rs, saVSX2  = rsVSX2  rs }
        sf = addExtension saveFile "new"
    BL.writeFile sf $ encodeLazy ss
    renameFile sf saveFile

-- Load the status from file, modify the given run state
loadStatus :: RunState -> IO RunState
loadStatus rs = do
    sfe <- doesFileExist saveFile
    if not sfe
       then return rs
       else do
           bs <- BL.readFile saveFile
           case decodeLazy bs of
               Left estr -> fail $ "Decode save file: " ++ estr
               Right sa  -> return rs { rsLast = saLast sa, rsVTPla = saVTPla sa,
                                        rsVSX  = saVSX  sa, rsVSX2  = saVSX2  sa }

-- Make an engine config line for one param & value
toEngConfLine :: (String, Double) -> String
toEngConfLine (pa, v) = pa ++ "=" ++ show (round v :: Int)

{--
baseDir :: String -> FilePath
baseDir session = learnDir </> session

-- Name and make current directories for reference and candidate
makeDirs :: FilePath -> String -> IO (String, String)
makeDirs base thread = do
    let refcurr = base </> ("ref" ++ thread)
        chacurr = base </> ("cha" ++ thread)
    createDirectoryIfMissing True chacurr
    return (refcurr, chacurr)
--}

-- Data types used in configuration:
data Engine = Engine {
                  engName, engProto :: Text,
                  engBin  :: FilePath
                  -- engArgs :: [Text]
              }

data Draw = Draw {
                drMoveNo, drMoveCnt, drScore :: Int
            }

data Resign = Resign {
                  reMoveCnt, reScore :: Int
              }

-- Simplified time control
-- Times in seconds, for now only integers
-- but it should be double, for very short time controls
data TimeControl = TimeControl {
                       tcMoves, tcTotal, tcPerMv :: Maybe Int
                   }

data Tour = Tour {
                toEvent   :: Text,
                toTimes   :: TimeControl,
                toRounds  :: Int,
                toHash    :: Int,
                toPgnIn   :: Maybe Text,
                toDraw    :: Draw,
                toResign  :: Resign,
                toThreads :: Maybe Int
            }

data Replay = Replay {
                  reStart, reRest :: Double
              }

data OptParam = OptParam {
                    opParam :: Text,
                    opLower, opUpper :: Double
                }

data Config = Config {
                  coCCC     :: FilePath,  -- how to start cutechess-cli (full path)
                  coIters   :: Int,
                  coNoise   :: Double,    -- it is actually noise to signal ratio (variances)
                  coReplay  :: Replay,
                  coTour    :: Tour,
                  coChaEng  :: Engine,
                  coRefEngs :: [Engine],
                  coParams  :: [OptParam]
              }

-- FromJSON interfaces for config related data types:
instance FromJSON Engine where
    parseJSON (Object v) = Engine <$>
                               v .: "engine" <*>
                               v .: "proto"  <*>
                               v .: "binary" -- <*>
                               -- v .: "args"
    parseJSON x           = error $ "Not en engine description: " ++ show x

instance FromJSON Draw where
    parseJSON (Object v) = Draw <$>
                              v .: "moveno"  <*>
                              v .: "movecnt" <*>
                              v .: "score"
    parseJSON x          = error $ "Not a draw description: " ++ show x

instance FromJSON Resign where
    parseJSON (Object v) = Resign <$>
                              v .: "movecnt" <*>
                              v .: "score"
    parseJSON x          = error $ "Not a tour description: " ++ show x

instance FromJSON TimeControl where
    parseJSON (Object v) = TimeControl <$>
                              v .:? "moves" <*>
                              v .:? "total" <*>
                              v .:? "per-move"
    parseJSON x          = error $ "Not a time control description: " ++ show x

instance FromJSON Tour where
    parseJSON (Object v) = Tour <$>
                              v .:  "event"   <*>
                              v .:  "time-control"  <*>
                              v .:  "rounds"  <*>
                              v .:  "hash"    <*>
                              v .:? "pgn-in"  <*>   -- optional
                              v .:  "draw"    <*>
                              v .:  "resign"  <*>
                              v .:?  "threads"
    parseJSON x          = error $ "Not a tour description: " ++ show x

instance FromJSON OptParam where
    parseJSON (Object v) = OptParam <$>
                              v .: "param" <*>
                              v .: "from"  <*>
                              v .: "to"
    parseJSON x          = error $ "Not an optimisation parameter: " ++ show x

instance FromJSON Replay where
    parseJSON (Object v) = Replay <$>
                              v .: "score"  <*>
                              v .: "fraction"
    parseJSON x          = error $ "Not a replay parameter: " ++ show x

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                              v .: "cutechess"  <*>
                              v .: "iterations" <*>
                              v .: "noise"      <*>
                              v .: "replay"     <*>
                              v .: "tour"       <*>
                              v .: "engine"     <*>
                              v .: "engines"    <*>
                              v .: "params"
    parseJSON x          = error $ "Error in config: " ++ show x

addCutechessPgnOut :: String -> FilePath -> FilePath -> [String] -> [String]
addCutechessPgnOut me engcf path args = pgnout ++ args
    where pgnout = [ "-pgnout", path, "-engine", "conf=" ++ me, "arg=-c", "arg=" ++ engcf ]

-- Return a list of parameters for the cutechess-cli command
mkCutechessCommand :: Config -> [String]
mkCutechessCommand config = site ++ event ++ conc ++ draw ++ resi ++ ttype ++ games ++ rounds
                            ++ open ++ rest ++ econf
    where tour   = coTour config
          site   = [ "-site", "Sixpack" ]
          event  = [ "-event", T.unpack (toEvent tour) ]
          conc   = case toThreads tour of
                       Just th -> [ "-concurrency", show th ]
                       Nothing -> []
          draw   = [ "-draw", "movenumber=" ++ show (drMoveNo $ toDraw tour),
                     "movecount=" ++ show (drMoveCnt $ toDraw tour),
                     "score=" ++ show (drScore $ toDraw tour) ]
          resi   = [ "-resign", "movecount=" ++ show (reMoveCnt $ toResign tour),
                     "score=" ++ show (reScore $ toResign tour) ]
          ttype  = [ "-tournament", "gauntlet" ]
          games  = [ "-games", "2" ]                          -- make configurable?
          rounds = [ "-rounds", show (toRounds tour) ]
          open   = case toPgnIn tour of
                       Just pgnin -> [ "-openings", "file=" ++ T.unpack pgnin, "order=random" ]
                       Nothing    -> []
          rest   = [ "-recover", "-each", "restart=on", "option.Hash=" ++ show (toHash tour),
                     "tc=" ++ timeControl (toTimes tour) ]
          econf  = concatMap (\e -> [ "-engine", "conf=" ++ T.unpack (engName e)]) $ coRefEngs config

timeControl :: TimeControl -> String
timeControl (TimeControl { .. })
    = case tcTotal of
          Nothing -> case tcPerMv of
                         Nothing -> error "Wrong time control, no times"
                         Just ps -> "0+" ++ show ps
          Just tc -> let tot = case tcMoves of
                                   Nothing -> show tc
                                   Just ms -> show tc ++ "/" ++ show ms
                     in case tcPerMv of
                            Nothing -> tot
                            Just pm -> tot ++ "+" ++ show pm

-- Run one tournament with cutechess-cli
oneMatch :: FilePath -> [String] -> String -> IO Rational
oneMatch cccBin args me = do
    (_, Just hout, _, ph)
        <- createProcess (proc cccBin args) { std_out = CreatePipe }
    hSetBuffering hout LineBuffering
    catch (everyLine me hout 0) $ \e -> do
        let es = ioeGetErrorString e
        if es == runFile
           then do
               -- Used deleted the running file: terminate with normal message
               terminateProcess ph
               ioError $ userError $ "Running file not present, stop"
           else do
               putStrLn $ "Error in everyLine: " ++ es
               terminateProcess ph
               -- We will have to repeat the experiment:
               oneMatch cccBin args me

-- waitMicroSec :: Int
-- waitMicroSec = 60 * 1000 * 1000  -- 60 seconds

-- Read the lines from the cutechess-cli pipe
everyLine :: String -> Handle -> Rational -> IO Rational
everyLine me h = go
    where go !r = do
              cfe <- doesFileExist runFile
              if not cfe
                 then ioError $ userError runFile
                 else do
                     {--
                     mlin <- timeout waitMicroSec $ hGetLine h
                     case mlin of
                         Nothing -> do
                             putStrLn "Timeout in everyLine"
                             go r
                         Just lin -> do
                     --}
                             lin <- hGetLine h
                             putStrLn $ "Got: " ++ lin
                             if "Rank Name " `isPrefixOf` lin
                                then return r
                                else if "Finished game " `isPrefixOf` lin
                                        then go (r + getScore lin me)
                                        else go r

-- The line has the following structure:
-- Finished game xxx (aaa vs bbb): rez ...
-- We rely here on the fact that our learning engine plays in all games
-- So if it was not white, then it is black
getScore :: String -> String -> Rational
getScore line me
    | rez == "1/2-1/2" = 1 % 2
    | me == white      = whiteRez
    | otherwise        = blackRez
    where (_:_:_:('(':white):_:_:rez:_) = words line
          (w:_:b:_) = rez
          whiteRez  = fromIntegral $ ord w - ord '0'
          blackRez  = fromIntegral $ ord b - ord '0'
