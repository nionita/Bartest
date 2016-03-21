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

import           Prelude hiding (catch)
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import qualified Data.ByteString.Char8 as B
import           Data.Char (isSpace)
import           Data.IORef
import           Data.List (intersperse, isPrefixOf, sortBy, groupBy, delete)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import           Data.Yaml
import           System.Directory
import           System.Environment (getArgs)
import           System.FilePath
import           System.IO
import           System.IO.Error hiding (catch)
import           System.Process
import           System.Random
import           System.Time (getClockTime)

-- Some constants for playing one match
cuteChessDir = "J:\\Chess\\cutechess-cli-0.6.0-win32"
cuteChessCom = cuteChessDir ++ "\\cutechess-cli.exe"

-- We have to use the first parameter from CLOP twofold:
-- first part will be a subdirectory in which the CLOP learn session will run
-- second part will a unique identifier for the parallel process
-- The 2 parts are delimitted by an underscore "_"
main = do
    (proc : seed : params)  <- getArgs
    let dict = makeDict params
        (session, thread) = break (== '_') proc
    res <- runGame session thread seed dict
    putStrLn res

-- Take a list of param/values sequence (as strings) and structure it
-- in form [(param, value)]
makeDict :: [String] -> [(String, Double)]
makeDict ss = go Nothing ss []
    where go Nothing [] acc = acc
          go _       [] _   = error "Unmatched param/value sequence"
          go Nothing  (p:ps) acc = go (Just p) ps acc
          go (Just p) (v:ps) acc = go Nothing  ps ((p, read v):acc)

-- This data has to be passed to our callback, so it can do its work
-- But we must change it during the callback, so we will pass a reference to it
data RunState = RunState {
                    rsCCC    :: FilePath,     -- how to start cutechess-cli
                    rsEvent  :: String,       -- event name used for file names
                    rsRoot   :: FilePath,     -- root directory, used to start there
                    rsParams :: [Strings],    -- param list needed to write engine config
                    rsLast   :: Int,          -- last run number
                    rsChaEng :: String,       -- the learning engine name
                    rsMaxPts :: Double        -- max number of points
                }

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
prepareGames :: FilePath -> IO RunState
prepareGames rootdir = do
    createDirectoryIfMissing True refcurr

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
    tod <- getClockTime
    rs  <- readIORef rsref
    let crtrun  = rsLast rs + 1
        runname = rsEvent rs ++ "-run-" ++ show crtrun
        logfile = rsRoot rs </> "logdir" </> (runname ++ ".log")
        pgnout  = rsRoot rs </> "pgnout" </> (runname ++ ".pgn")
        engcfg  = rsRoot rs </> ("cwd-" ++ challenger) </> (runname ++ ".cfg")
    -- write a new log file:
    writeFile logfile $ "New run " ++ runname ++ " started at "
        ++ (calendarTimeToString $ toCalendarTime tod) ++ "\n"
    -- write engine config file:
    writeFile engcfg $ unlines $ map toEngConfLine $ zip rsParams $ V.toList v
    setCurrentDirectory $ rsRoot rs
    let args = addCutechessPgnOut pgnout (rsArgs rs)
    pts <- fromIntegral <$> oneMatch (rsCCC rs) args (rsChaEng rs)
    writeIORef rsref $ rs { rsLast = crtrun }
    -- return difference to maximum, so that minimize will maximize the performance
    return $! (rsMaxPts rs - pts) / rsMaxPts rs

-- Learning engine name as a string
challenger :: Config -> String
challenger = T.unpack . engName . coChaEng

-- Make an engine config line for one param & value
toEngConfLine :: (OptParam, Double) -> String
toEngConfLine (op, v) = T.unpack (opParam op) ++ "=" ++ show (round v :: Int)

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
                  engBin  :: FilePath,
                  engArgs :: [Text]
              }

data Draw = Draw {
                drMoveNo, drMoveCnt, drScore :: Int
            }

data Resign = Resign {
                  rsMoveCnt, rsScore :: Int
              }

-- Simplified time control
-- Times in seconds, for now only integers
-- but it should be double, for very short time controls
data TimeControl = TimeControl {
                       tcMoves, tcTotal, tcPerMv :: Maybe Int,
                   }

data Tour = Tour {
                toEvent   :: Text,
                toTimes   :: TimeControl,
                toPgnIn   :: Maybe FilePath,
                toDraw    :: Draw,
                toResign  :: Resign,
                toThreads :: Int
            }

data OptParam = OptParam {
                    opParam :: Text,
                    opLower, opUpper :: Double
                }

data Config = Config {
                  coCCC     :: FilePath,	-- how to start cutechess-cli (full path)
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
                               v .: "binary" <*>
                               v .: "cwd"    <*>
                               v .: "args"
    parseJSON x           = error $ "Not en engine description: " ++ show x

instance FromJSON Draw where
    fromJSON (Object v) = Draw <$>
                              v .: "moveno" <*>
                              v .: "movecnt" <*>
                              v .: "score"
    fromJSON x          = error $ "Not a draw description: " ++ show x

instance FromJSON Resign where
    fromJSON (Object v) = Resign <$>
                              v .: "mvecnt" <*>
                              v .: "score"
    fromJSON x          = error $ "Not a tour description: " ++ show x

instance FromJSON TimeControl where
    fromJSON (Object v) = TimeControl <$>
                              v .:? "moves" <*>
                              v .:? "total" <*>
                              v .:? "per-move"
    fromJSON x          = error $ "Not a time control description: " ++ show x

instance FromJSON Tour where
    fromJSON (Object v) = Tour <$>
                              v .:  "event"   <*>
                              v .:  "time-control"
                              v .:? "pgn-in"  <*>   -- optional
                              v .:  "pgn-out" <*>
                              v .:  "draw"    <*>
                              v .:  "resign"
    fromJSON x          = error $ "Not a tour description: " ++ show x

instance FromJSON OptParam where
    fromJSON (Object v) = OptParam <$>
                              v .: "param" <*>
                              v .: "from"  <*>
                              v .: "to"
    fromJSON x          = error $ "Not an optimisation parameter: " ++ show x

instance FromJSON Config where
    fromJSON (Object v) = Config <$>
                              v .: "cutechess" <*>
                              v .: "tour"      <*>
                              v .: "engine"    <*>
                              v .: "engines"   <*>
                              v .: "params"
    fromJSON x          = error $ "Error in config: " ++ show x

addCutechessPgnOut :: FilePath -> [String] -> [String]
addCutechessPgnOut path args = pgnout ++ args
    where pgnout = [ "-pgnout", path ]

-- Return a list of parameters for the cutechess-cli command
mkCutechessCommand :: Config -> [String]
mkCutechessCommand config = site ++ event ++ cont ++ draw ++ resi ++ ttype ++ games ++ rounds
                            ++ open ++ pgnout ++ rest ++ econf
    where tour   = coTour config
          site   = [ "-site", "Sixpack" ]
          event  = [ "-event", T.unpack (toEvent tour) ]
          conc   = case toThreads tour of
                       Just th -> [ "-concurrency", show th ]
                       Nothing -> []
          draw   = [ "-draw", "movenumber=" ++ show (drMoveNo $ toDraw tour),
                     "movecount=" ++ show (drMoveCnt $ toDraw tour),
                     "score=" ++ show (drScore $ toDraw tour) ]
          resi   = [ "-resign", "movecount=" ++ show (rsMoveCnt $ toResign tour),
                     "score=" ++ show (rsScore $ toResign tour) ]
          ttype  = [ "-tournament", "gauntlet" ]
          games  = [ "-games", "2" ]                          -- make configurable?
          rounds = [ "-rounds", show (toRounds tour) ]
          open   = case toPgnIn tour of
                       Just pgnin -> [ "-openings", "file=" ++ T.unpack pgnin, "order=random" ]
                       Nothing    -> []
          rest   = [ "-recover", "-each", "restart=on", "option.Hash=" ++ show (toHash tour),
                     "tc=", timeControl tour ]
          econf  = concatMap (\e -> [ "-engine", "conf=" ++ T.unpack (engName e)])
                       $ coChaEng config : coRefEngs config
          eng1 = [	-- the learning engine
              "-engine",
              "name=" ++ takeFileName (dcChaEngine dcf),
              "cmd=" ++ dcChaEngine dcf,
              "dir=" ++ chacurr,
              "proto=" ++ dcChaProto dcf
              ] ++ words chatime
                ++ map (\(n,v) -> "arg=-p" ++ n ++ "=" ++ show v) dict
                ++ optArgs dcf dcChaArgs
          eng2 = [	-- the reference engine
              "-engine",
              "name=" ++ takeFileName (dcRefEngine dcf),
              "cmd=" ++ dcRefEngine dcf,
              "dir=" ++ refcurr,
              "proto=" ++ dcRefProto dcf
              ] ++ words reftime
                ++ optArgs dcf dcRefArgs
          args = if white then common ++ eng1 ++ eng2 else common ++ eng2 ++ eng1
          pgnout = base </> ("thr" ++ thread ++ ".pgn")
          -- When we give depth, this has priority and no time control is requested
          chatime | dcChaDepth dcf /= 0   = "tc=inf depth=" ++ show (dcChaDepth dcf)
                  | null (dcChaMoves dcf) = "tc=" ++ dcChaFixTm dcf ++ "+" ++ dcChaSecPerMv dcf
                  | otherwise             = "tc=" ++ dcChaMoves dcf ++ "/"
                                               ++ dcChaFixTm dcf ++ "+" ++ dcChaSecPerMv dcf
          reftime | dcRefDepth dcf /= 0   = "tc=inf depth=" ++ show (dcRefDepth dcf)
                  | null (dcRefMoves dcf) = "tc=" ++ dcRefFixTm dcf ++ "+" ++ dcRefSecPerMv dcf
                  | otherwise             = "tc=" ++ dcRefMoves dcf ++ "/"
                                               ++ dcRefFixTm dcf ++ "+" ++ dcRefSecPerMv dcf

timeControl :: TimeControl -> String
timeControl (TimeControl { .. })
    = case tcTotal of
          Nothing -> case tcPerMv of
                         Nothing -> error "Wrong time control, no times"
                         Just ps -> "0+" ++ show ps
          Just to -> let tot = case tcMoves of
                                   Nothing -> show to
                                   Just ms -> show to ++ "/" ++ show ms
                     in case tcPerMv of
                            Nothing -> tot
                            Just pm -> tot ++ "+" ++ show pm

oneMatch :: FilePath -> [String] -> String -> IO Int
oneMatch cccBin args me = do
    (_, Just hout, _, ph)
            <- createProcess (proc cccBin args) { std_out = CreatePipe }
    catch (everyLine me hout 0) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        throwIO e

everyLine :: String -> Handle -> Rational -> IO Rational
everyLine me h = go
    where go !r = do
              lin <- hGetLine h
              -- when debug $ putStrLn $ "Got: " ++ lin
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

optArgs dcf f = if null (f dcf) then [] else map (\w -> "arg=" ++ w) (words $ f dcf)

readDriverConfig = stringToConfig <$> readFile "ClopDriver.txt"

stringToConfig :: String -> DriverConfig
stringToConfig = foldr (\(n, s) dc -> lookApply n s dc funlist) defDC
                       . catMaybes . map readParam . noComments . lines
    where defDC = DC {
              dcRefEngine = "J:\\Barbarossa\\dist\\build\\Barbarossa\\Barbarossa_0_01_k3nmd.exe",
              dcRefArgs   = "-l5",
              dcChaEngine = "J:\\Barbarossa\\dist\\build\\Barbarossa\\Barbarossa_0_01_castp.exe",
              dcChaArgs   = "-l2",
              -- dcChaConfig = "J:\\AbaAba\\dist\\build\\Abulafia\\test1-51-6.txt",
              -- dcRefMoves = "40", dcRefFixTm = "20", dcRefSecPerMv = "0.2",
              -- dcChaMoves = "40", dcChaFixTm = "20", dcChaSecPerMv = "0.2",
              dcRefDepth = 0,
              dcChaDepth = 0,
              dcRefMoves = "", dcRefFixTm = "120", dcRefSecPerMv = "1",
              dcChaMoves = "", dcChaFixTm = "120", dcChaSecPerMv = "1",
              dcRefProto = "uci", dcChaProto = "uci",
              dcAnlFile  = "",	-- when those are both defined (/="" and /=0)
              dcAnlCount = 0	-- then the analysis version of the "game" is performed
          }
          setRefEngine   s dc = dc { dcRefEngine   = s }
          setRefArgs     s dc = dc { dcRefArgs     = s }
          setRefMoves    s dc = dc { dcRefMoves    = s }
          setRefFixTm    s dc = dc { dcRefFixTm    = s }
          setRefSecPerMv s dc = dc { dcRefSecPerMv = s }
          setRefDepth    s dc = dc { dcRefDepth    = read s }
          setRefProto    s dc = dc { dcRefProto    = s }
          setChaEngine   s dc = dc { dcChaEngine   = s }
          setChaArgs     s dc = dc { dcChaArgs     = s }
          setChaMoves    s dc = dc { dcChaMoves    = s }
          setChaFixTm    s dc = dc { dcChaFixTm    = s }
          setChaSecPerMv s dc = dc { dcChaSecPerMv = s }
          setChaDepth    s dc = dc { dcChaDepth    = read s }
          setChaProto    s dc = dc { dcChaProto    = s }
          setAnlFile     s dc = dc { dcAnlFile     = s }
          setAnlCount    s dc = dc { dcAnlCount    = read s }
          funlist = [ ("RefEngine",   setRefEngine),
                      ("RefArgs",     setRefArgs),
                      ("RefMoves",    setRefMoves),
                      ("RefFixTm",    setRefFixTm),
                      ("RefDepth",    setRefDepth),
                      ("RefSecPerMv", setRefSecPerMv),
                      ("RefProto",    setRefProto),
                      ("ChaEngine",   setChaEngine),
                      ("ChaArgs",     setChaArgs),
                      ("ChaMoves",    setChaMoves),
                      ("ChaFixTm",    setChaFixTm),
                      ("ChaDepth",    setChaDepth),
                      ("ChaSecPerMv", setChaSecPerMv),
                      ("ChaProto",    setChaProto),
                      ("AnlFile",     setAnlFile),
                      ("AnlCount",    setAnlCount) ]
          noComments = filter (not . isComment)
          isComment ""                 = True
          isComment ('-':'-':_)        = True
          isComment (c:cs) | isSpace c = isComment cs
          isComment _                  = False

type Setter a = String -> a -> a

lookApply :: String -> String -> a -> [(String, Setter a)] -> a
lookApply s v a = maybe a (($ a) . ($ v)) . lookup s

readParam :: String -> Maybe (String, String)
readParam s = let (ns, vs) = span (/= '=') s
              in case vs of
                     ('=' : rs) -> Just (strip ns, strip rs)
                     _          -> Nothing	-- did not contain '='
    where strip = filter (not . isSpace)
