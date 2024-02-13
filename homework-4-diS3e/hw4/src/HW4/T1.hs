module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Data.Maybe (fromJust, isJust)
import Control.Monad

newtype ExceptState e s a
  = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f a = ES $ mapExcept (mapAnnotated f) . runES a

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ wrapExcept . (a :# )

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES gs) = ES $ \s -> joinExcept $ mapExcept (\((ES fs) :# ss) -> fs ss) (gs s)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ wrapExcept . (() :#) . f

throwExceptState :: e -> ExceptState e s a
throwExceptState = ES . const . Error

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (ES fs) <*> a = ES $ \s ->
    case fs s of
      Error e -> Error e
      Success (f :# ss) -> runES (mapExceptState f a) ss


instance Monad (ExceptState e s) where
  a >>= f = joinExceptState $ fmap f a

data EvaluationError = DivideByZero
  deriving Show

binaryOp :: (Double -> Double -> Prim Double) ->
            (Double -> Double -> Double) ->
            Maybe (Double -> Double -> Bool, EvaluationError) ->
            Expr ->
            Expr ->
            ExceptState EvaluationError [Prim Double] Double
binaryOp constructor f argCheck x y =
  do { a <- eval x
     ; b <- eval y
     ; when (isJust argCheck && fst (fromJust argCheck) a b)
        (throwExceptState $ snd $ fromJust argCheck)
     ; modifyExceptState (constructor a b :)
     ; return $ a `f` b }

unaryOp :: (Double -> Prim Double) ->
           (Double -> Double) ->
           Expr ->
           ExceptState EvaluationError [Prim Double] Double
unaryOp constructor f x =
  do { a <- eval x
     ; modifyExceptState (constructor a :)
     ; return $ f a }

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = binaryOp Add (+) Nothing x y
eval (Op (Sub x y)) = binaryOp Sub (-) Nothing x y
eval (Op (Mul x y)) = binaryOp Mul (*) Nothing x y
eval (Op (Div x y)) = binaryOp Div (/)
  (Just (const (== 0), DivideByZero)) x y
eval (Op (Abs x)) = unaryOp Abs abs x
eval (Op (Sgn x)) = unaryOp Sgn signum x

