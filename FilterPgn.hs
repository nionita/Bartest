module Main where

import Control.Monad (forM_, when)
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
engine = P.many (P.alphaNum <|> P.char '-')

res :: Parser String
res = P.try (P.string "0-1") <|> P.try (P.string "1/2-1/2")
         <|> P.try (P.string "1-0") <|> P.string "*"

game :: Parser Entry
game = do
    w <- enclosed '[' ']' $ literal "White" >> P.space >> quoted engine
    _ <- P.space
    b <- enclosed '[' ']' $ literal "Black" >> P.space >> quoted engine
    _ <- P.space
    q <- enclosed '[' ']' $ literal "Result" >> P.space >> quoted res
    _ <- P.space
    r <- res
    return $ Entry { white = w, black = b, hres = q, result = r }

parseEntry :: String -> Either P.ParseError Entry
parseEntry = P.parse game ""

-- Filter "pgn" files, i.e. accept:
-- either games where both players are in the given list (player1 player2 ...)
-- or games where none of the players are in the given list (-v player1 player2 ...)
-- We can filter only pgn files which contain for every game only the players and the result,
-- i.e. the results.pgn file which we use for rating
main :: IO ()
main = do
    args <- getArgs
    glss <- grpLines . zip [1..] . lines <$> readFile "results.pgn"
    case args of
        "-v" : rest -> forM_ glss $ perGame (\w b -> not ((w `elem` rest) || (b `elem` rest)))
        _           -> forM_ glss $ perGame (\w b -> (w `elem` args) && (b `elem` args))

perGame :: (String -> String -> Bool) -> (Int, Int, [String]) -> IO ()
perGame f (ri, ro, ls) = do
    let gs = concat $ intersperse " " ls
    case parseEntry gs of
        Left err -> do
            putStrLn $ "\n*** Lines " ++ show ri ++ " to " ++ show ro
            putStrLn $ show err
            putStrLn gs
        Right g  -> when (result g /= "*" && f (white g) (black g)) $ do
                        mapM_ putStrLn ls
                        putStrLn ""

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
