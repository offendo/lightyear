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
--  |             Input class and helpers              |
--  +--------------------------------------------------+
--{{{
-- Wrapper for input text
data Input = Input {
             current :: !String,
             line :: !Int,
             col  :: !Int,
             rest :: !String
             } deriving (Show, Eq)


-- Move input to next character, track col/line position
incr :: Input -> Input
incr (Input s l c []) = Input s 0 0 ""
incr (Input s l c a) = case head a == '\n' of
                        True  -> Input cline (l + 1) 0 (tail a)
                        False -> Input s l (c + 1) (tail a)
   where cline = takeWhile (/= '\n') (tail a)

-- Helper function for testing
fromstr :: String -> Input
fromstr s = Input s 0 0 s
-- }}}
--  +--------------------------------------------------+
--  |             Types for error tracking             |
--  +--------------------------------------------------+
-- {{{

data Message = Unexpected !String
             | Primitive !String
             | Expected !String
             | General !String

-- For comparison 
instance Enum Message where
    fromEnum (Unexpected _) = 0
    fromEnum (Primitive  _) = 1
    fromEnum (Expected   _) = 2
    fromEnum (General    _) = 3
    toEnum _ = error "toEnum not defined for Message"

instance Eq Message where
    a == b = (fromEnum a) == (fromEnum b)

-- Extract strings inside each Message
instance Show Message where
    show (Unexpected s) = s
    show (Primitive  s) = s
    show (Expected   s) = s
    show (General    s) = s

-- Error type; Currently only has ParseError but will encapsulate more (i.e., conflicting types,
-- no function exists, etc.)
data LYError = ParseError String Int Int [Message]

instance Show LYError where-- {{{
    -- add new instance for each type of error!
    show (ParseError s l c msgs) =
        ("parse error at position " 
        ++ show l ++ ":" ++ show c ++ "\n")
        ++ '\n': line ++ '\n':uline 
        ++ (concat $ map (' ':) $ clean $ [show_prims, show_unexp, show_exp, show_gen])
        where line   = show l ++ " | " ++ takeWhile (/= '\n') s
              uline  = (replicate (c + 3 + (length $ show l)) ' ') ++ "^" 
              prims   = filter ((Primitive "")  ==) msgs
              unexps  = filter ((Unexpected "") ==) msgs
              exps    = filter ((Expected "")   ==) msgs
              gens    = filter ((General "")    ==) msgs
              show_prims | not (null unexps) ||
                           null prims = ""
                         | otherwise  = "\n<!> unexpected " ++ (show $ head prims)
              show_unexp = show_msgs "\n<!> unexpected" unexps
              show_exp   = show_msgs "\n<?> when looking for" exps
              show_gen   = show_msgs "" gens 

-- Error printing helper functions {{{
              show_msgs :: String -> [Message] -> String
              show_msgs prefix [] = ""
              show_msgs prefix [m] = prefix ++ ' ' : show m
              show_msgs prefix [m,n] = prefix ++ ' ' : show m ++ " or " ++ show n
              show_msgs prefix ms = prefix ++ " one of: " ++ (newlines all)
                  where all = clean $ map show ms 
              newlines :: [String] -> String
              newlines ls = intercalate "\n\t- " ("":ls)
              commas :: [String] -> String
              commas ls = intercalate ", " ls
              clean :: [String] -> [String]
              clean = nub . filter (not . null)
-- }}}

-- }}}


-- Appends an error message to the error
add_msg :: LYError -> Message -> LYError
add_msg (ParseError s l c msgs) msg = ParseError s l c (msg:msgs)

cmp_pair :: (Int, Int) -> (Int, Int) -> Int
cmp_pair (a,b) (c,d)
    | a > c = 1
    | c > a = -1
    | b > d = 1
    | d > b = -1
    | otherwise = 0

merge_errs :: LYError -> LYError -> LYError
merge_errs e@(ParseError s l c msgs) e'@(ParseError s' l' c' msgs') = 
    case cmp_pair (l,c) (l',c') of 
      0  -> ParseError s l c (msgs ++ msgs')
      1  -> ParseError s l c msgs
      -1 -> ParseError s' l' c' msgs'

-- Checks if there are any error messages
is_unknown :: LYError -> Bool
is_unknown (ParseError _ _ _ msgs) = null $ tail msgs

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
  empty = Parser $ \inp@(Input s l c r) -> Left (ParseError s l c [Primitive $ show $ head r])
  p <|> q = Parser $ \inp@(Input s l c r) -> case parse p inp of
                              Right r -> Right r
                              Left e@(ParseError s' l' c' msgs)  -> 
                                  case cmp_pair (l', c') (l, c) of
                                     0  -> case parse q inp of
                                            Right r -> Right r
                                            Left e' -> Left (merge_errs e e')
                                     _  -> Left e

instance Monad Parser where
    p >>= f = Parser $ \inp -> case parse p inp of
                                Left e          -> Left e
                                Right (inp', a) -> parse (spaces *> f a) inp'
-- }}}
--  +--------------------------------------------------+
--  |                 Helper functions                 |
--  +--------------------------------------------------+
-- {{{

-- Adds an error message to the parser if it fails
(<?>) :: Parser a -> String -> Parser a
p <?> m = Parser $ \inp@(Input s l c a) ->
            case parse p inp of
             Right r  -> Right r
             Left (ParseError s l c msgs)  -> Left $ 
                 ParseError s l c $ Expected m : filter ((Expected "") /=) msgs

try :: Parser a -> Parser a
try p = Parser $ \inp -> case parse p inp of
                           Right r -> Right r
                           Left (ParseError _ _ _ msgs) -> Left $ ParseError s l c msgs
                               where s = current inp
                                     l = line inp
                                     c = col inp

-- If char satisfies input function, parse, otherwise Nothing
sat :: (Char -> Bool) -> Parser Char
sat f = Parser $ g
    where g (Input s l c []) = Left $ ParseError s l c [Primitive "'end-of-input'"]
          g inp@(Input s l c a) = case f $ head a of
                                 True -> Right (incr inp, head a)
                                 False -> Left $ ParseError s l c [Primitive $ show $ head a]


-- Attempts to parse with p, returns default value v otherwise
option :: Parser a -> a -> Parser a
option p v = Parser $ \inp -> case parse p inp of
                                Left e  -> Right (inp, v)
                                Right r -> Right r

-- Parse while function is satisfied
whilef :: (Char -> Bool) -> Parser String
whilef f = many $ sat f

-- Parse until function is satisfied
untilf :: (Char -> Bool) -> Parser String
untilf f = whilef (\x -> not $ f x)

sep :: Parser a -> Parser b -> Parser [a]
sep a b = do h <- a
             t <- many (b >> a)
             return $ h:t
          <|> return []

sep1 :: Parser a -> Parser b -> Parser [a]
sep1 a b = (:) <$> a <*> (many $ b >> a)

endwith :: Parser a -> Char -> Parser a
endwith p c = p <* whilef notc <* char c
    where notc x = (isSpace x) && (x /= c)

notfollowedby :: Show a => Parser a -> Parser Char 
notfollowedby p = Parser $ \inp@(Input s l c r) -> case parse p inp of
                                     Right (_, t) -> Left $ (ParseError s l c [Primitive $ show t])
                                     Left e -> Right (inp, ' ')

untilc :: (Char -> Bool) -> Char -> Parser String
untilc f c = whilef (\x -> f x && (x /= c))

surround :: Char -> Char -> Parser b -> Parser b
surround open close p = (spaces >> char open) *> p <* (spaces >> char close)

parens :: Parser a -> Parser a
parens p = surround '(' ')' p

-- }}}
--  +--------------------------------------------------+
--  |                Parser primatives                 |
--  +--------------------------------------------------+
-- {{{
--
char :: Char -> Parser Char
char ch = sat (==ch)

string :: String -> Parser String
string s = (sequence $ map char s) <?> show s

digit :: Parser Char
digit = sat isDigit

alpha :: Parser Char
alpha = sat isAlpha

upper :: Parser Char
upper = sat isUpper

lower :: Parser Char
lower = sat isLower

alnum :: Parser Char
alnum = sat isAlphaNum <?> "an alpha-numeric"

spaces :: Parser String
spaces = whilef isSpace

name :: Parser String
name = (:) <$> alpha <*> many alnum <?> "an identifier."

anychar :: Parser Char
anychar = sat (\x -> True)

eof :: Parser Char 
eof = notfollowedby anychar <?> "'end-of-input'"

newline :: Parser String
newline = (untilc isSpace '\n' <* (char '\n' <|> eof )) <?> "a new line '\\n'"
-- }}}

