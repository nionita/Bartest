{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Char
import Data.List (intersperse)
import System.Environment (getArgs)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (Parser, (<|>))

data Entry = Entry { white, black, hres, result :: String } deriving Show

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed oc cc p = do
    _ <- P.char oc
    a <- p
    _ <- P.char cc
    return a

quoted :: Parser a -> Parser a
quoted = enclosed '"' '"'

literal :: String -> Parser ()
literal s = P.string s >> return ()

engine :: Parser String
engine = P.many P.alphaNum

res :: Parser String
res = P.try (P.string "0-1") <|> P.try (P.string "1/2-1/2")
         <|> P.try (P.string "1-0")

game :: Parser Entry
game = do
    w <- enclosed '[' ']' $ literal "White" >> P.space >> quoted engine
    P.space
    b <- enclosed '[' ']' $ literal "Black" >> P.space >> quoted engine
    P.space
    q <- enclosed '[' ']' $ literal "Result" >> P.space >> quoted res
    P.space
    r <- res
    return $ Entry { white = w, black = b, hres = q, result = r }

parseEntry :: String -> Either P.ParseError Entry
parseEntry = P.parse game ""

debug = False

main = do
    args <- getArgs
    -- This function determines which condition should a game satisfy
    -- in order to remain in the output pgn
    -- Params are the white and black players
    let f = case args of
                "-v" : rest -> \w b -> not ((w `elem` rest) || (b `elem` rest))
                _           -> \w b -> (w `elem` args) && (b `elem` args)
    glss <- grpLines . zip [1..] . lines <$> readFile "results.pgn"
    mapM_ (perGame f) glss

perGame :: (String -> String -> Bool) -> (Int, Int, [String]) -> IO ()
perGame f (ri, ro, ls) = do
    let gs = concat $ intersperse " " ls
    case parseEntry gs of
        Left err -> when debug $ do
            putStrLn $ "\n*** Lines " ++ show ri ++ " to " ++ show ro
            putStrLn $ show err
            putStrLn gs
        Right g  -> if f (white g) (black g)
                       then do
                           mapM_ putStrLn ls
                           putStrLn ""
                       else return ()

-- Group the lines per game, with beginning and ending line numbers
grpLines :: [(Int, String)] -> [(Int, Int, [String])]
grpLines []     = []
grpLines (il@(i,l) : ils)
    | emptyLine l = grpLines ils
    | otherwise   = let j = fst . last $ il : ns
                    in (i, j, l : map snd ns) : grpLines rs
    where (ns, rs) = break (emptyLine . snd) ils

emptyLine :: String -> Bool
emptyLine ""     = True
emptyLine (c:cs) = isSpace c && emptyLine cs
