{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[] = 'False
  Contains name (name ': xs) = 'True
  Contains name (x ': xs) = Contains name xs

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete v (v ': xs) = xs
  Delete v (x ': xs) = x ': Delete v xs
  Delete v '[] = '[]


type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v (v ': xs) = v ': xs
  Add v (x ': xs) = x ': Add v xs
  Add v '[] = '[v]