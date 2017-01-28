{-# LANGUAGE BangPatterns #-}
module Main where

-- This is used to index a text file so that
-- a program which wants to go directly to line x
-- can read the index and skip directly to the
-- begining of that line
-- I use it to index fen files for SelfPlay

-- import Control.Applicative ((<$>))
import Control.Monad (when)
import Foreign
import System.Environment (getArgs)
import System.FilePath
import System.IO

bufferSize :: Word32
bufferSize = 1024 * 10  -- 10k words

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> error "File to index is required as argument"
        _:_:_ -> error "Too many arguments"
        n:_   -> withFile n ReadMode $ \ifi -> do
            if takeExtension n == "idx"
               then error "Given file looks already like an index"
               else do
                   let idxfile = replaceExtension n "idx"
                   nlCnt <- nlCount ifi
                   withBinaryFile idxfile WriteMode $ \ofi -> do
                       allocaBytes (fromIntegral bufferSize * 4) $ \ptr -> copyIdx ifi ofi nlCnt ptr

copyIdx :: Handle -> Handle -> Word32 -> Ptr Word32 -> IO ()
copyIdx hi ho nl buf = go 0 0
    where go !i !l
              | i == bufferSize = do
                  hPutBuf ho buf (fromIntegral $ bufferSize * 4)
                  go 0 l
              | otherwise = do
                  eof <- hIsEOF hi
                  if eof
                     then when (i > 0) $ hPutBuf ho buf (fromIntegral $ i * 4)
                     else do
                         pokeElemOff buf (fromIntegral i) l
                         line <- hGetLine hi
                         go (i+1) (l + nl + fromIntegral (length line))

-- We want to know how long the newline of the input file is
-- (LF == 1, CRLF == 2)
nlCount :: Handle -> IO Word32
nlCount h = do
    -- Get initial position, to restore it at the end
    pos0 <- hGetPosn h
    -- Set no newline translation, which is considering only a NL
    hSetNewlineMode h noNewlineTranslation
    line0 <- hGetLine h
    hSetPosn pos0
    hSetNewlineMode h nativeNewlineMode
    line1 <- hGetLine h
    -- Restore the initial position
    hSetPosn pos0
    let l0 = length(line0)
        l1 = length(line1)
    if l0 == l1
       then return 1
       else if l0 == l1 + 1
               then return 2
               else error "Can't determine newline mode of the file"
