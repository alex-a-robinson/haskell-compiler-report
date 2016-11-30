
import Data.Char
import Control.Applicative()
import Control.Monad (liftM, ap)


newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
    return t = Parser $ \s -> [(t, s)]
    m >>= k  = Parser $ \s -> [(x, y) | (u, v) <- parse m s, (x, y) <- parse (k u) v]

data Term = Con Int | Div Term Term
            deriving (Show)

parseTerm :: String -> Term
parseTerm = fst . head . parse term

term :: Parser Term
term = do
    t <- factor
    term' t

term' :: Term -> Parser Term
term' t = divFactor `bchoice` return t
    where divFactor = do
            lit '/'
            u <- factor
            term' $ Div t u

factor :: Parser Term
factor = numTerm `choice` parenTerm
    where numTerm = do
            n <- number
            return $ Con n
          parenTerm = do
            lit '('
            t <- term
            lit ')'
            return t

zero :: Parser a
zero = Parser $ \s -> []

choice :: Parser a -> Parser a -> Parser a
m `choice` n = Parser $ \s -> parse m s ++ parse n s

-- Biased choice
bchoice :: Parser a -> Parser a -> Parser a
m `bchoice` n = Parser $ \s -> if null (parse m s) then parse n s else parse m s

filt :: Parser a -> (a -> Bool) -> Parser a
m `filt` p = do
    t <- m
    if p t then return t else zero

item :: Parser Char
item = Parser item'
    where item' [] = []
          item' (a : x) = [(a, x)]

digit :: Parser Char
digit = item `filt` isDigit

lit :: Char -> Parser Char
lit c = item `filt` \c' -> c == c'

reiterate :: Parser a -> Parser [a]
reiterate m = multiple `bchoice` return []
    where multiple = do
            t <- m
            ts <- reiterate m
            return $ t : ts

number :: Parser Int
number = do
    ds <- reiterate digit
    return (read ds :: Int)

main = print $ parseTerm "1972/2/23"
