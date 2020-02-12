module Lightyear
    where

--  +--------------------------------------------------+
--  |                     Imports                      |
--  +--------------------------------------------------+
-- {{{
import           Control.Applicative
import           Control.Monad
import           Data.Char
-- }}}
--  +--------------------------------------------------+
--  |          Input type for error tracking           |
--  +--------------------------------------------------+
--{{{
data Input = Input {
             rest :: !String,
             line :: !Int,
             col  :: !Int
             } deriving (Show, Eq)

incr :: Input -> Input
incr inp = case h of 
               '\n' -> Input t (l + 1) 0
               _    -> Input t l (c + 1)
    where h = head $ rest inp
          t = tail $ rest inp
          l = line inp 
          c = col inp 

fromstr :: String -> Input
fromstr s = Input s 0 0
--}}}
--  +--------------------------------------------------+
--  |       Parser type and typeclass instances        |
--  +--------------------------------------------------+
-- {{{

newtype Parser a = Parser {
    parse :: Input -> (Input, Maybe a)
}

instance Functor Parser where
  fmap f p = Parser $ \inp -> case parse p inp of
                                (inp', Just x) -> (inp', Just $ f x)
                                (_, Nothing)   -> (inp, Nothing)

instance Applicative Parser where
  pure x = Parser $ \inp -> (inp, Just x)
  p <*> q = Parser $ \inp -> do
                    case parse p inp of
                      (_, Nothing)   -> (inp, Nothing)
                      (inp', Just f) -> parse (f <$> q) inp'

instance Alternative Parser where
  empty = Parser $ \inp -> (inp, Nothing)
  p <|> q = Parser $ \inp -> case parse p inp of
                               (inp', Just x) -> (inp', Just x)
                               (_, Nothing)   -> parse q inp

instance Monad Parser where
    p >>= f = Parser $ \inp -> case parse p inp of
                            (_, Nothing)   -> (inp, Nothing)
                            (inp', Just a) -> parse (f a) inp'
-- }}}
--  +--------------------------------------------------+
--  |                 Helper functions                 |
--  +--------------------------------------------------+
-- {{{
-- If char satisfies input function, parse, otherwise Nothing
sat :: (Char -> Bool) -> Parser Char
sat f = Parser $ g
    where g inp = if (f h) then (incr inp, Just h) else (inp, Nothing)
                           where h = head $ rest inp 


-- Attempts to parse with p, returns default value v otherwise
perhaps :: Parser a -> a -> Parser a
perhaps p v = Parser $ \inp -> case parse p inp of
                                 (inp', Just v') -> (inp', Just v')
                                 (_, Nothing)         -> (inp, Nothing)

-- Parse while function is satisfied
whilef :: (Char -> Bool) -> Parser String
whilef f = many $ sat f

-- Parse until function is satisfied
untilf :: (Char -> Bool) -> Parser String
untilf f = whilef (\x -> not $ f x)
-- }}}
--  +--------------------------------------------------+
--  |                Parser primatives                 |
--  +--------------------------------------------------+
-- {{{
char :: Char -> Parser Char
char c = Parser $ g
        where h = head . rest
              g inp = if c == h inp 
                         then (incr inp, Just c) 
                         else (inp, Nothing)
               

string :: String -> Parser String
string s = sequenceA $ map char s
--string [] = return []
--string (h:t) = Parser $ \inp -> 
--    case parse (char h) inp of
--      (inp', Just c) -> parse (string t) inp'
--      (inp, Nothing) -> (inp, Nothing)


digit :: Parser Char
digit = sat isDigit

alpha :: Parser Char
alpha = sat isAlpha

alnum :: Parser Char
alnum = sat isAlphaNum

space :: Parser Char
space = sat isSpace

spaces :: Parser String
spaces = many space

identifier :: Parser String
identifier = (:) <$> alpha <*> many alnum
-- }}}

