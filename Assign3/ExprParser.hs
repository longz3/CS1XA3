{-|
Module : ExprDiff
Description : Contains a type class and instances for differentiable expressions
          The function that the Parser support：
          <expression> ::= <expression> | <expression> + <term> | <expression> - <term>
          <term> ::= <term> | <term> * <factor> | <term> / <factor>
          <factor> ::= <functions> | <negative-expr> | <float-number> | <int> | <variable> | <paren-expr>
          <function> ::= sin <factor> | cos <factor> | log <factor> | exp <factor>
          <negative-expr> ::= - <factor>
          <paren-expr> ::= ( <expression> )


          
          
          -- parserExprD and parserExprF are simplified parserExpr for double and float.


Stability : experimental
Portability : POSIX
-}

module ExprParser (parseExprD, parseExprF, parseExpr) where

import           ExprType

import           Control.Applicative hiding (Const, many, (<|>))
import           Text.Parsec
import           Text.Parsec.String

-- | Parses a string into an Expr Double type
parseExprD = parseExpr :: String -> Expr Double
-- | Parses a string into an Expr Float type
parseExprF = parseExpr :: String -> Expr Float

-- | Parses a string into an Expr a type
parseExpr :: (Floating a, Read a) => String -> Expr a
parseExpr ss = case parse expr "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Expr parser. Expr is formed by terms seperated by '+' or '-'
expr :: (Floating a, Read a) => Parser (Expr a)
expr = chainl1 term $ try spacesAdd
  where add = (char '+' >> return Add)
          <|> (char '-' >> return ((. negateExpr) . Add))
        spacesAdd = do
            spaces
            op <- add
            spaces
            return op

-- | Term parser. Term is formed by factors seperated by '*' or '/'
term :: (Floating a, Read a) => Parser (Expr a)
term = chainl1 factor (try spaceMult)
  where mult = (char '*' >> return Mult)
           <|> (char '/' >> return Div)
        spaceMult = do
            spaces
            op <- mult
            spaces
            return op

{- | Factors are mininimum elements. It can be
-   1. Function call (e.g., sin, cos)
-   2. A negated expression
-   3. A floating point number
-   4. An integer number
-   5. A variable
-   6. An expression surrounded by parenthesis
-}
factor :: (Floating a, Read a) => Parser (Expr a)
factor = unary "sin" Sin
     <|> unary "cos" Cos
     <|> unary "log" Log
     <|> unary "exp" Exp
     <|> unary "-" negateExpr
     <|> try float
     <|> number
     <|> variable
     <|> parenExpr


parenExpr :: (Floating a, Read a) => Parser (Expr a)
parenExpr = do
  char '('
  skipMany space
  e <- expr
  skipMany space
  char ')'
  return e

unary :: (Floating a, Read a) => String -> (Expr a -> Expr a) -> Parser (Expr a)
unary s func = do
  string s
  skipMany space
  e <- factor
  return $ func e

float :: (Floating a, Read a) => Parser (Expr a)
float = Const . read <$> all
    where int = minus <|> number
          all = (++) <$> int <*> ((:) <$> char '.' <*> many digit)
          minus  = (:) <$> char '-' <*> number
          number = many1 digit

number :: (Floating a, Read a) => Parser (Expr a)
number = Const . read <$> (minus <|> number)
    where minus  = (:) <$> char '-' <*> number
          number = many1 digit

-- | Variables start with letter or _, followed by several letter, digit, or _
variable :: Parser (Expr a)
variable = do
  first <- letter <|> char '_'
  rest <- many $ letter <|> char '_' <|> digit
  return . Var $ first : rest
