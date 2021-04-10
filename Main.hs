module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import qualified Text.ParserCombinators.ReadP as P

data Tok = CNum Int | CVar String | CBool Bool | CString String | CPrint Tok | CEquals deriving (Show, Eq)

fromJust :: Maybe a -> a
fromJust (Just a) = a

fromIndex :: Int -> [a] -> [a]
fromIndex _ [] = []
fromIndex 0 a = a
fromIndex n (x:xs)
    | n > 0     = fromIndex (n - 1) xs
    | otherwise = xs

untilIndex :: Int -> [a] -> [a]
untilIndex _ [] = []
untilIndex n (x:xs)
    | n > 0     = x : untilIndex (n-1) xs
    | otherwise = []

digit :: P.ReadP Char
digit = P.choice [
                P.char '1', P.char '2', P.char '3',
                P.char '4', P.char '5', P.char '6',
                P.char '7', P.char '8', P.char '9',
                            P.char '0']

printC :: P.ReadP [String]
printC = (:) <$>    (P.string "print" <* P.skipSpaces)
             <*>    ((flip (:) []) <$> P.between (P.char '(') (P.char ')') (P.many $ P.satisfy (\x -> x /= ')')))
             <*     (P.many ((P.char '\n') <|> (P.char ';')))

stringC :: P.ReadP [String]
stringC = ((:) "str") <$> ( (flip (:) []) <$> P.between (P.char '"') (P.char '"') (P.many $ P.satisfy (\x -> x /= '"')))

varC :: P.ReadP [String]
varC = ((:) "var") <$> ((flip (:) []) <$> P.between (P.char '|') (P.char '|') (P.many $ P.satisfy (\x -> x /= '|')) <* P.skipSpaces)

equalC :: P.ReadP [String]
equalC = ((:) "equals") <$>
    ((flip (:) []) <$> P.string "=")

parseC :: P.ReadP [Tok]
parseC = do
    toks <- P.many $ P.choice [(flip (:) $ []) <$> (P.many1 digit)
                    , (flip (:) $ []) <$> P.string "true"
                    , (flip (:) $ []) <$> P.string "false"
                    , printC
                    , stringC
                    , (flip (:) $ []) <$> P.string " "
                    , (flip (:) $ []) <$> P.string "\n"
                    , varC
                    , equalC]
    return $ f toks
  where f :: [[String]] -> [Tok]
        f [] = []
        f (x:xs)
            | x == ["true"]                 = CBool True                                                            : f xs
            | x == ["false"]                = CBool False                                                           : f xs
            | head x == "str"               = CString (last x)                                                      : f xs
            | head x == "print"             = (CPrint . last . fst . last) (P.readP_to_S parseC (last x))           : f xs
            | head x == "var"               = CVar (last x)                                                         : f xs
            | (and . map isDigit) (head x)  = CNum (read (head x))                                                  : f xs
            | head x == "equals"            = CEquals                                                               : f xs
            | head x == " "                 =                                                                         f xs
            | head x == "\n"                =                                                                         f xs

execC :: [Tok] -> [(String, Tok)] -> IO ()
execC [] _ = return ()
execC (tok:toks) vars = case tok of
    (CPrint str)    -> putStrLn (f str) >> execC toks vars
    (CNum n)        -> execC toks vars
    (CString s)     -> execC toks vars
    (CVar n)        -> case toks of
                            (CEquals:CEquals:a) -> fail "Can't use equal sign right after an equal sign"
                            (CEquals:val:toks') -> execC toks' ((n, val) : vars)
                            _                   -> fail "Unexpected syntax"
  where f :: Tok -> String
        f (CNum n)      = show n
        f (CString n)   =      n
        f (CVar n)
            | (lookup n vars) == Nothing    = error ("Variable \"" ++ n ++ "\" is not defined")
            | otherwise                     = f $ fromJust $ lookup n vars
        f a             = fail ("You can't print " ++ show a)

prompt :: String -> IO ()
prompt text = do
    putStr text
    hFlush stdout
    inp <- getLine
    execC (fst $ last $ (P.readP_to_S (parseC) inp)) []
    return ()

main :: IO ()
main = do
    args <- getArgs
    if (length args > 0) then do
        content <- readFile $ head args
        execC (fst $ last $ (P.readP_to_S (parseC) content)) []
    else
        prompt "C > " >> main
