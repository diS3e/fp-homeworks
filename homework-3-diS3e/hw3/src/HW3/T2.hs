{-# LANGUAGE TupleSections #-}
module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  , (++.)
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (Some a, y) = mapOption (a,) y

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P x y) = P (a, x) (b, y)

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q w x y z) = Q (a, w) (b, x) (c, y) (d, z)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# x, b :# y) = (a, b) :# x <> y

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error x, _) = Error x
distExcept (Success x, b) = mapExcept (x,) b

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, x) = mapPrioritised (a,) x
distPrioritised (x, Low a) = mapPrioritised (,a) x
distPrioritised (Medium a, x) = mapPrioritised (a,) x
distPrioritised (x, Medium a) = mapPrioritised (,a) x
distPrioritised (High a, High b) = High (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> xs, y :> ys) = (x, y) :> distStream (xs, ys)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

infixr 5 ++.

(++.) :: List a -> List a -> List a
Nil ++. y = y
(x :. xs) ++. y = x :. xs ++. y

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (x :. xs, y) = mapList (x,) y ++. distList (xs, y)

wrapList :: a -> List a
wrapList = (:. Nil)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F $ \i -> (f i, g i)

wrapFun :: a -> Fun i a
wrapFun x = F $ \_ -> x
