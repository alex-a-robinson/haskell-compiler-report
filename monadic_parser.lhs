 # Basic monadic parser
**Aim:** Implement a basic monadic parser to show understanding

Based on [Monadic Parsers: Implementing a micro Parsec](http://olenhad.me/articles/monadic-parsers/)

 ## TODO
- clean up documentaiton, examples, funciton names, make my own
- Clean up monad code, some not needed

 ## Introduction
What is a parser, it is a function which takes a string and outputs a list of tuples of results and remaining string to parse.


> module MP where
>
> import Data.Char (isDigit)
> import Control.Applicative()
> import Control.Monad (liftM, ap)
>
> data Parser a = Parser (String -> [(a, String)])
> item :: Parser Char
> item = Parser (\s -> case s of
>                       "" -> []
>                       (c:cs) -> [(c, cs)])

This little parser consumes the first character in a string returning that character as a result, it fails by returning an empty string if the string is empty.

Now we'll define a function which applies a Parser. It extracts the funciton from a parser and in order to apply it.

> parse :: Parser t -> String -> [(t, String)]
> parse (Parser p) = p

> data Term = Num Int
>           | Add Term Term
>           | Sub Term Term
>           | Mul Term Term
>           | Div Term Term
>             deriving (Show)

What if we want to apply two parsers together, i.e. funciton composition f(g(x)). We call this binding.

NOTE: due to linting warnings this has been modified from the original, check it still works. `bind p f = Parser (\s -> concat $ map (\ (a, s') -> parse (f a) s') $ parse p s)`

> bind :: Parser a -> (a -> Parser b) -> Parser b
> bind p f = Parser (concatMap (\ (a, s') -> parse (f a) s') . parse p)

So reading backwords, first apply Parser p on the String s, which returns a list of tuples. Then map over the list and apply the function f to the result a, then parse the reminang string s' with the new parser we got from `(f a)`, then concat to flatten the list.

Next we define `uint` a function accepts any value and produces a Parser which takes nothing and returns that value

> uint :: a -> Parser a
> uint a = Parser (\s -> [(a, s)])

Using our newly created building blocks `bind` and `uint` we can start creating combinators. First we define `satisfies`, it accepts a predicate and returns a parser which consumes a character if the predicate is satisifed.

> satisfies :: (Char -> Bool) -> Parser Char
> satisfies p = item `bind` \c -> if p c then uint c else Parser (const [])

`item` from before consumes the first character and returns it, this is passed to a lambda which applies the predicat `p` on the character and returns a Parser (constructed from uint) if successful otherwise a Parser of an empty list denoting failure.

We have just implemented a Parser monad, `uint` is our `return` and `bind` our `>>=`, lets refactor a little.

[Functor-Applicative-Monad Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)

> instance Functor Parser where
>   fmap = liftM
>
> instance Applicative Parser where
>   pure  = return
>   (<*>) = ap
>
> instance Monad Parser where
>   return a = Parser (\s -> [(a, s)])
>   p >>= f  = Parser (concatMap (\ (a, s') -> parse (f a) s') . parse p)

Now we have our sweet parser, lets implement some combinators

> class Monad m => MonadPlus m where
>   mzero :: m a
>   mplus :: m a -> m a -> m a
>
> instance MonadPlus Parser where
>   mzero = Parser (const [])
>   mplus p q = Parser (\s -> parse p s ++ parse q s)
>
> choice :: Parser a -> Parser a -> Parser a
> choice p q = Parser (\s -> case parse (mplus p q) s of
>                                [] -> []
>                                (x:_) -> [x])

`MonadPlus` type class is a common pattern, `mzero` denotes failure, `mplus` concats the result from two Parsers (as failure is `[]`, then if first parser files it concats result of second parser to `[]`). As mostly we only care about the first result we have the combinator `choice` which applies mplus and returns the first result if success, or [] on failure.

More combinators, `char` takes a character and consumes and returns if present otherwise fails. `string` takes a string and consumes that string if found in the input.

> char :: Char -> Parser Char
> char c = satisfies (c ==)
>
> string :: String -> Parser String
> string "" = return ""
> string (c:cs) = do {_ <- char c; _ <- string cs; return (c:cs)}

> many :: Parser a -> Parser [a]
> many p = some p `choice` return []
>
> some :: Parser a -> Parser [a]
> some p = do {a <- p; as <- many p; return (a:as)}
>
> sepBy :: Parser a -> Parser b -> Parser [a]
> p `sepBy` sep = (p `sepBy1` sep) `choice` return []
>
> sepBy1 :: Parser a -> Parser b -> Parser [a]
> p `sepBy1` sep = do a <- p
>                     as <- many (do {_ <- sep; p})
>                     return (a:as)

`many` (0 or more) applications of Parser `p`. `some` (1 or more) is the same. `sepBy` is many applications of `p` separated by applications of Parser `sep` the values of which are chucked.

 ## Lexical Parsing
Lexical phase = tokenising input

> space :: Parser String
> space = many (satisfies isSpace)
>     where isSpace ' '  = True
>           isSpace '\n' = True
>           isSpace '\t' = True
>           isSpace _    = False
>
> token :: Parser a -> Parser a
> token p = do {a <- p; _ <- space; return a}
>
> symb :: String -> Parser String
> symb s = token (string s)
>
> digit :: Parser Char
> digit = satisfies isDigit
>
> number :: Parser Int
> number = do cs <- some digit
>             return $ read cs

An example, which parses the string "THING" seperated by the string "SEPERATOR". Note this won't work if there are trailing spaces after each "THING", we could use `token $ symb "THING"` which eats up trailing spaces for us.
```haskell
MP> parse ((symb "THING") `sepBy` (symb "SEPERATOR")) "THINGSEPERATORTHINGSEPERATORTHINGLEFT_OVER"
[(["THING","THING","THING"],"LEFT_OVER")]
```

`space` consumes any whitespace. `token` returns a token ignoring trailing whitespae. `digit` parsers a single digit. `number` parsers integers.

> chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
> chainl p op a = (p `chainl1` op) `choice` return a
>
> chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
> p `chainl1` op = do {a <- p; rest a}
>                  where rest a = (do f <- op
>                                     b <- p
>                                     rest (f a b))
>                                 `choice` return a

`chainl1` parses repeated application of a parser `p` seperated by a parser `op` the result value is used to combine the result from the `p` parsers.

 ## Basic Grammar

NOTE we apply mulop then addop for BODMAS

> expr :: Parser Term
> expr = term `chainl1` addop
>
> addop :: Parser (Term -> Term -> Term)
> addop = do {_ <- symb "+"; return Add} `choice` do {_ <- symb "-"; return Sub}
>
> mulop :: Parser (Term -> Term -> Term)
> mulop = do {_ <- symb "*"; return Mul} `choice` do {_ <- symb "/"; return Div}
>
> term :: Parser Term
> term = factor `chainl1` mulop
>
> factor :: Parser Term
> factor = numTerm `choice` parenTerm
>   where numTerm = do {a <- number; _ <- space; return $ Num a}
>         parenTerm = do {_ <- symb "("; n <- expr; _ <- symb ")"; return n}
>
>
>
> run :: String -> IO()
> run s = case head $ parse expr s of
>           (ast, "")  -> print ast
>           (ast, err) -> do {print ast; putStrLn ("Error parsing next character '" ++ [head err] ++ "'.")}

Note can't deal with negative numbers!
