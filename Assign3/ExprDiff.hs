{-|
Module : ExprDiff
Description : Contains a type class and instances for differentiable expressions
Stability : experimental
Portability : POSIX
MacID: longz3
StudentID:400137638
Email: aldrichlong22@gmail.com

-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           ExprParser

class DiffExpr a where
  -- | Evaluate an expression given var values
  eval :: M.Map String a -> Expr a -> a
  -- | Simplify an expression and sub in values
  simplify :: M.Map String a -> Expr a -> Expr a
  -- | Perform partial differention w.r.t identifier
  partDiff :: String -> Expr a -> Expr a

  infixl 6 !+
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify emptyMap $ Add e1 e2

  infixl 7 !*
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify emptyMap $ Mult e1 e2

  infixl 7 !/
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify emptyMap $ Div e1 e2

  val :: a -> Expr a
  val = Const
  var :: String -> Expr a
  var = Var


emptyMap :: (Ord k) => M.Map k a
emptyMap = M.fromList []

-- auxiliary operator:
infixl 6 !-
(!-) :: (Floating a, Eq a) => Expr a -> Expr a -> Expr a
e1 !- e2 = simplify emptyMap $ Add e1 $ negateExpr e2

instance (Floating a, Eq a) => DiffExpr a where
  -- Binary operators evals left-hand side and right-hand side and operate on the final value
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Div e1 e2)  = eval vrs e1 / eval vrs e2
  -- Base case
  eval vrs (Const x) = x
  eval vrs (Var x) = fromMaybe (error "failed lookup in eval") $ M.lookup x vrs
  eval vrs (Sin e) = sin $ eval vrs e
  eval vrs (Cos e) = cos $ eval vrs e
  eval vrs (Log e) = log $ eval vrs e
  eval vrs (Exp e) = exp $ eval vrs e

  -- | convert Var to Const if it's in vrs
  simplify vrs v@(Var x) = maybe v Const $ M.lookup x vrs
  -- | Const is keep unchanged
  simplify _ v@(Const _) = v
  simplify vrs (Mult f g)
    | l == Const 0 = Const 0  -- ^ left is 0, so answer is 0
    | r == Const 0 = Const 0  -- ^ right is 0, so answer is 0
    | l == Const 1 = r        -- ^ left is 1, answer is rightside
    | r == Const 1 = l        -- ^ rightside is 1, so answer is left side
    | isConst l && isConst r = Const $ getConst l * getConst r -- both are constant, then mulitply
    | otherwise = l `Mult` r  
    where l = simplify vrs f
          r = simplify vrs g
  simplify vrs (Div f g)
    | l == Const 0 = Const 0                -- ^ if the divided number is 0
    | isConst l && isConst r = Const $ getConst l / getConst r -- ^ both consts
    | isConst r = Const (1 / getConst r) !* l   -- ^ divided by const
    | l == r = Const 1
    | otherwise = l `Div` r
    where l = simplify vrs f
          r = simplify vrs g
  simplify vrs (Add f g)
    | l == Const 0 = r                  -- ^ anything add to 0 is itself
    | r == Const 0 = l
    | isConst l && isConst r = Const $ getConst l + getConst r -- ^ both const
    | otherwise = l `Add` r
    where l = simplify vrs f
          r = simplify vrs g
  simplify vrs (Sin e)
    | isConst e' = Const $ sin $ getConst e'-- ^ when sin function is a constant
    | otherwise = Sin e'                    -- ^ not a contant
    where e' = simplify vrs e
  simplify vrs (Cos e)
    | isConst e' = Const $ cos $ getConst e' -- ^ when cos fucntion is a constant
    | otherwise = Cos e'                     -- ^ not a constant
    where e' = simplify vrs e
  simplify vrs (Log (Exp e)) = simplify vrs e -- ^ exp(log(x)) == x
  simplify vrs (Log e)
    | isConst e' = Const $ log $ getConst e' -- ^ If x is constant, then find the answer
    | otherwise = Log e'                     -- ^ unable to simplify
    where e' = simplify vrs e
  
  simplify vrs (Exp (Log x)) = simplify vrs x -- ^ exp and log are inverse function, so exp(log(x))==x
  simplify vrs (Exp e)
    | isConst e' = Const $ exp $ getConst e' 
    | otherwise = Exp e'                     
    where e' = simplify vrs e


  -- Addition differentiation: (f + g)' == f' + g'
  partDiff v (Add f g)  = partDiff v f !+ partDiff v g
  -- (f * g)' == f'g + g'f
  partDiff v (Mult f g) = (partDiff v f !* g) !+ (partDiff v g !* f)
  -- (f/g)' == (f'g - g'f)/g^2
  partDiff v (Div f g)  = (partDiff v f !* g !- partDiff v g !* f) !/ Mult g g
  -- c' == 0
  partDiff v (Const _)  = Const 0
  -- partial derivatives of x
  partDiff v (Var x)    = Const $ if x == v then 1 else 0
  
  -- sin' x = cos x
  partDiff v (Sin e)    = Cos e !* partDiff v e
  -- cos' x = -sin x
  partDiff v (Cos e)    = negateExpr (Sin e) !* partDiff v e
  -- ln' x = 1/x
  partDiff v (Log e)    = Const 1 !/ e !* partDiff v e
  -- (e^x)' = e^x
  partDiff v (Exp e)    = Exp e !* partDiff v e

