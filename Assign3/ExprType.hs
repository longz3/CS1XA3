module ExprType where

import           Data.List

-- | A datatype for common numeric expression
data Expr a = Add (Expr a) (Expr a)     -- ^ Binary Add
            | Mult (Expr a) (Expr a)    -- ^ Binary multiplication
            | Div (Expr a) (Expr a)     -- ^ Binary division
            | Const a                   -- ^ Constant value
            | Var String                -- ^ Identifier
            | Sin (Expr a)              -- ^ Sin function
            | Cos (Expr a)              -- ^ Cos function
            | Log (Expr a)              -- ^ Log function
            | Exp (Expr a)              -- ^ Exp function
  deriving Eq

instance Show a => Show (Expr a) where
  -- | Pretty printing show expression
  show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
  show (Div e1 e2)  = parens (show e1) ++ " / " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " + " ++ parens (show e2)
  show (Var ss)     = ss
  show (Const x)    = show x
  show (Sin e)      = "sin" ++ parens (show e)
  show (Cos e)      = "cos" ++ parens (show e)
  show (Log e)      = "log" ++ parens (show e)
  show (Exp e)      = "exp" ++ parens (show e)

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | get all identifiers in expression
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]

-- | negate a expression by multiply it with -1
negateExpr :: Floating a => Expr a -> Expr a
negateExpr = Mult (Const $ negate 1)

-- | Tell whether a Expr is const value
isConst :: Expr a -> Bool
isConst (Const _) = True
isConst _         = False

-- | Get const value
getConst :: Expr a -> a
getConst (Const a) = a
getConst _         = error "not a const"
