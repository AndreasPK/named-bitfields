{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Data.BitPacking.TypeMachinery
    ( ListIndex )
where

-- import Data.Bits as Bits
import GHC.Exts
-- import GHC.Int
-- import Data.Kind
-- import GHC.Exts
import GHC.TypeError
import GHC.TypeLits
-- import Data.Proxy

type ListIndex :: [Symbol] -> Symbol -> Nat -> Nat
type family ListIndex (fields :: [Symbol]) (s :: Symbol) (n :: Nat) where
    ListIndex ( s : _  ) s n = n :: Nat
    ListIndex ( _ : xs ) s n = ListIndex xs s (n+1)
    ListIndex '[] _ _ = TypeError (Text "Index not found")

