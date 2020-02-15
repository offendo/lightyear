module Buzz
    where

--  +--------------------------------------------------+
--  |                     Imports                      |
--  +--------------------------------------------------+
-- {{{

import           Control.Applicative
import           Data.Char
import           Data.List
import           Lightyear

-- }}}

--  +--------------------------------------------------+
--  |                  Helper parsers                  |
--  +--------------------------------------------------+
-- {{{
upper :: Parser Char
upper = sat isUpper

lower :: Parser Char
lower = sat isLower
-- }}}

--  +--------------------------------------------------+
--  |                  AST data types                  |
--  +--------------------------------------------------+
-- {{{

-- Expr is an expression which can be evaluated
data Expr = Paren Expr             -- expression in brackets
          | FuncCall String [Expr] -- function name and args
          | Name String            -- variable name/function as data
          | Literal String         -- literal string or number or something

-- Stmt is a statement which does some action 
data Stmt = If       Expr [Stmt] [Stmt] -- if pred then actions else actions
          | Let      Decl Expr          -- let variable: Type = value
          | Function Decl [Decl]        -- def join (strs: [String]) with (d: String)
          | While    Expr [Stmt]        -- while pred do actions
          | For      String Expr [Stmt] -- for var in list do actions
          | Import   String             -- import module

-- Decl is a shorthand for a type declaration, i.e. name: Type
-- Also can be name1 (args) name2 (args) : Type 
data Decl = OneDecl {
                dname :: String,
                dtype :: String
            } | SplitDecl {
                dnames :: [String],
                dtype  :: String
            }
-- }}}

--  +--------------------------------------------------+
--  |                 AST Type Parsers                 |
--  +--------------------------------------------------+
--  {{{
-- stmt     :: if-stmt  | let-stmt | while-loop | for-loop | func-def | return
-- expr     :: ( expr ) | funccall | name       | value
-- funccall :: infix    | prefix
-- infix    :: expr (name expr)+
-- prefix   ::      (name expr)+
-- type dec :: varname: typename
-- typename :: Xxxx
-- varname  :: xxxx


-- type names must start with an upper-case character
tname :: Parser String
tname = ((:) <$> upper <*> (many alnum)) <?> "a <type name>"

-- variable and function names must start with lower-case charcter
fname :: Parser String
fname = ((:) <$> lower <*> (many alnum)) <?> "a <function name>"

vname :: Parser String
vname = ((:) <$> lower <*> (many alnum))  <?> "a <variable name>"

-- }}}
