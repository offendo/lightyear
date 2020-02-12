module Lightyear
    where

--  +--------------------------------------------------+
--  |                     Imports                      |
--  +--------------------------------------------------+
-- {{{
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
-- }}}
--  +--------------------------------------------------+
--  |             Types for error tracking             |
--  +--------------------------------------------------+
--{{{

data Input = Input {
             line :: !Int,
             col  :: !Int,
             rest :: !String
             } deriving (Show, Eq)

data Message = Expected !String
             | Unexpected !String
             | Primative !String
             | Message !String

instance Show Message where
    -- add new instance for each message type
    show (Expected s)   = "when attempting to parse " ++ s ++ "\n"
    show (Unexpected s) = "Unexpected '" ++ s ++ "' "
    show (Primative s)  = "Unexpected '" ++ s ++ "' "
    show (Message s)    = s ++ "\n"

data LYError = ParseError Int Int [Message]

instance Show LYError where
    -- add new instance for each type of error!
    show (ParseError l c msgs) =
        ("\n**ParseError** at position " ++ show l ++ ":" ++ show c ++ "\n") ++ (concat $ map (\msg ->  (show msg)) msgs)

add_msg :: LYError -> Message -> LYError
add_msg (ParseError l c msgs) msg = ParseError l c (msgs ++ [msg])

is_unknown :: LYError -> Bool
is_unknown (ParseError _ _ msgs) = null msgs

incr :: Input -> Input
incr (Input l c []) = Input 0 0 ""
incr (Input l c a) = case head a == '\n' of
                       True  -> Input (l + 1) 0 (tail a)
                       False -> Input l (c + 1) (tail a)

fromstr :: String -> Input
fromstr = Input 0 0

--}}}
--  +--------------------------------------------------+
--  |       Parser type and typeclass instances        |
--  +--------------------------------------------------+
-- {{{

newtype Parser a = Parser {
    parse :: Input -> Either LYError (Input, a)
}

instance Functor Parser where
  fmap f p = Parser $ \inp -> case parse p inp of
                                Left e          -> Left e
                                Right (inp', a) -> Right (inp', f a)

instance Applicative Parser where
  pure x = Parser $ \inp -> Right (inp, x)
  p <*> q = Parser $ \inp -> case parse p inp of
                              Left e          -> Left e
                              Right (inp', f) -> parse (f <$> q) inp'

instance Alternative Parser where
  empty = Parser $ \inp@(Input l c r) -> Left (ParseError l c [Primative [head r]])
  p <|> q = Parser $ \inp -> case parse p inp of
                              Left e          -> parse q inp
                              Right (inp', a) -> Right (inp', a)

instance Monad Parser where
    p >>= f = Parser $ \inp -> case parse p inp of
                                Left e          -> Left e
                                Right (inp', a) -> parse (f a) inp'
-- }}}
--  +--------------------------------------------------+
--  |                 Helper functions                 |
--  +--------------------------------------------------+
-- {{{

-- Adds an error message to the parser if it fails
(<?>) :: Parser a -> String -> Parser a
p <?> s = Parser $ \inp@(Input l c a) ->
                        case parse p inp of
                         Right r -> Right r
                         Left err -> case is_unknown err of
                                       True  -> Left $ ParseError l c [Expected s]
                                       False -> Left $ add_msg err (Expected s)


-- If char satisfies input function, parse, otherwise Nothing
sat :: (Char -> Bool) -> Parser Char
sat f = Parser $ g
    where g (Input l c []) = Left $ ParseError l c [Primative "end of file"]
          g inp@(Input l c a) = case f $ head a of
                                 True -> Right (incr inp, head a)
                                 False -> Left $ ParseError l c [Primative [head a]]


-- Attempts to parse with p, returns default value v otherwise
optional :: Parser a -> a -> Parser a
optional p v = Parser $ \inp -> case parse p inp of
                                Left e  -> Right (inp, v)
                                Right r -> Right r

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
char ch = sat (==ch) <?> show ch

string :: String -> Parser String
string s = (sequenceA $ map char s) <?> show s

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
identifier = ((:) <$> alpha <*> many alnum) <?> "an identifier."
-- }}}

