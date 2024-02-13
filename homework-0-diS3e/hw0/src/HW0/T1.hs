{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)  

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f s) = Iso s f 

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

aToBPair :: (a, (b, c)) -> ((a, b), c)
aToBPair (a, (b, c)) = ((a, b), c)

bToAPair :: ((a, b), c) -> (a, (b, c))
bToAPair ((a, b), c) = (a, (b, c))

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso aToBPair bToAPair

aToBEither :: Either a (Either b c) -> Either (Either a b) c
aToBEither (Left a) = Left . Left $ a
aToBEither (Right (Left b)) = Left . Right $ b
aToBEither (Right (Right c)) = Right c

bToAEither :: Either (Either a b) c -> Either a (Either b c)
bToAEither (Left (Left a)) = Left a
bToAEither (Left (Right b)) = Right . Left $ b
bToAEither (Right c) = Right . Right $ c

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso aToBEither bToAEither
