module Lightyear
    where

import           Control.Applicative
import           Control.Monad
import           Data.List



--  +--------------------------------------------------+
--  |       Parser type and typeclass instances        |
--  +--------------------------------------------------+
newtype Parser a = Parser {
    parse :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> do
                              (inp', x) <- p inp
                              Just (inp', f x)

instance Applicative Parser where
  pure x = Parser $ \inp -> Just (inp, x)

  p <*> q = Parser $ \inp -> do
                    case parse p inp of
                        Nothing        -> Nothing
                        Just (inp', f) -> parse (f <$> q) inp'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p <|> q = Parser $ \inp -> parse p inp <|> parse q inp

instance Monad Parser where
    p >>= f = Parser $ \inp -> do
                    case parse p inp of
                        Nothing        -> Nothing
                        Just (inp', a) -> parse (f a) inp'


--  +--------------------------------------------------+
--  |                Parser primatives                 |
--  +--------------------------------------------------+
