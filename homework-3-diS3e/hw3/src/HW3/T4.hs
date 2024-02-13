module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import Control.Monad (ap, join)
import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState = fmap 

wrapState :: a -> State s a
wrapState = return

joinState :: State s (State s a) -> State s a
joinState = join

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = (<*>) . pure

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  (S sa) >>= f = S $ \s ->
    let (a :# e) = sa s
        (S sb) = f a
    in sb e
  return a = S $ \s -> a :# s

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) x y = Op $ Add x y
  (-) x y = Op $ Sub x y
  (*) x y = Op $ Mul x y
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  (/) x y = Op $ Div x y
  fromRational = Val . fromRational

binaryOp :: (Double -> Double -> Prim Double) -> 
            (Double -> Double -> Double) -> 
            Expr -> 
            Expr -> 
            State [Prim Double] Double
binaryOp constructor f x y = 
  do { a <- eval x
     ; b <- eval y
     ; modifyState (constructor a b :)
     ; return $ a `f` b }

unaryOp :: (Double -> Prim Double) ->
           (Double -> Double) ->
           Expr ->
           State [Prim Double] Double
unaryOp constructor f x =
  do { a <- eval x
     ; modifyState (constructor a :)
     ; return $ f a }


eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = binaryOp Add (+) x y
eval (Op (Sub x y)) = binaryOp Sub (-) x y
eval (Op (Mul x y)) = binaryOp Mul (*) x y
eval (Op (Div x y)) = binaryOp Div (/) x y
eval (Op (Abs x)) = unaryOp Abs abs x
eval (Op (Sgn x)) = unaryOp Sgn signum x
