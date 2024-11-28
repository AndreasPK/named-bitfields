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

{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeApplications #-}

module Data.BitPacking.PackedFields where

import Data.Bits as Bits
import GHC.Exts
import GHC.Int
import Data.Kind
import GHC.Exts
import GHC.TypeError
import GHC.TypeLits
import Data.Proxy
import Data.Type.Ord as Type

import GHC.Word

import Data.BitPacking.TypeMachinery


type PackedSize :: a -> Nat
type family PackedSize a where
    PackedSize Bool = 1
    PackedSize (Maybe a) = 1 + PackedSize a
    PackedSize (a,b) = PackedSize a + PackedSize b
    PackedSize (Either a b) = 1 + (Type.Max (PackedSize a) (PackedSize b))

    PackedSize Word8 = 8
    PackedSize Word16 = 16
    PackedSize Word32 = 32

    PackedSize Int8 = 8
    PackedSize Int16 = 16
    PackedSize Int32 = 32

    PackedSize a = TypeError (Text "Missing PackedSize instance:" :$$:
                              ShowType a)

type PackedFieldOffset :: Symbol -> [(Symbol,Type)] -> Nat
type family PackedFieldOffset elem list where
    PackedFieldOffset elem list = PackedFieldOffset' elem list 0

type PackedFieldOffset' :: Symbol -> [(Symbol,Type)] -> Nat -> Nat
type family PackedFieldOffset' fld list n where
    PackedFieldOffset' fld ('(fld,t):_) n = n
    PackedFieldOffset' fld ('(_,t):xs) n = PackedFieldOffset' fld xs (PackedSize t + n)
    PackedFieldOffset' fld _ _ = TypeError
                                    (Text "Packed field does not contain field:" :$$:
                                     ShowType fld)

type PackedFieldType :: Symbol -> [(Symbol,Type)] -> Type
type family PackedFieldType fld list where
    PackedFieldType fld ('(fld,t):_) = t
    PackedFieldType fld ('(_,t):xs) = PackedFieldType fld xs
    PackedFieldType fld _ = TypeError
                                    (Text "Packed field does not contain field:" :$$:
                                     ShowType fld)

type PackedFieldsSize :: [(Symbol,Type)] -> Nat
type family PackedFieldsSize list where
    PackedFieldsSize ('(_,t):xs) = (PackedSize t) + (PackedFieldsSize xs)
    PackedFieldsSize '[] = 0
    -- PackedFieldType _ = TypeError
    --                                 (Text "Packed field does not contain field:" :$$:
    --                                  ShowType fld)



-- | A way to store things in other things.
--
-- All offsets and sizes are in *bits*
--
--
--
class PureStaticStorable store elem where
    poke :: store -> Int -> elem -> store
    peek :: store -> Int -> elem
    -- type PackedFieldOffset' :: store -> elem -> Int


-- Works as long as size elem <= size store
pokeBits :: forall elem store. (Bits store, FiniteBits elem, Integral elem, Num store)
         => store -> Int -> elem -> store
pokeBits store offset x =
        -- .....val....
    let shifted_val = (fromIntegral x :: store) `shiftL` offset
        -- zero out bits where we place the value
        -- 111110001111
        mask = complement $ (fromIntegral (oneBits :: elem) :: store) `shiftL` offset
        -- 11001___1011
        openStore = (store .&. mask)
    in  openStore .|. shifted_val

-- Works as long as @fromIntegral@ zeros bits beyond the size of elem
peekBits :: forall elem store. (Bits store, Num store, Integral store, Num elem)
         => store -> Int -> elem
peekBits store offset =
    fromIntegral $ store `shiftR` offset

instance PureStaticStorable Word64 Word8 where
    poke :: Word64 -> Int -> Word8 -> Word64
    poke = pokeBits

    peek :: Word64 -> Int -> Word8
    peek = peekBits

instance PureStaticStorable Word64 Word16 where
    poke = pokeBits
    peek = peekBits

instance PureStaticStorable Word64 Word32 where
    poke = pokeBits
    peek = peekBits

-- Takes a function that creates a bit pattern in store format
-- instead of using Num/Integral etc.
{-# INLINE pokeFromBits #-}
pokeFromBits :: forall elem store. (Bits store, FiniteBits elem)
         => (elem -> store) -> store -> Int -> elem -> store
pokeFromBits writer store offset x =
        -- .....val....
    let shifted_val = (writer x :: store) `shiftL` offset
        -- zero out bits where we place the value
        -- 111110001111
        mask = complement $ (writer (oneBits :: elem) :: store) `shiftL` offset
        -- 11001___1011
        openStore = (store .&. mask)
    in  openStore .|. shifted_val

{-# INLINE peekFromBits #-}
-- Takes a function that reads the bit pattern at the start of the store
-- instead of using Num/Integral etc.
peekFromBits :: forall elem store. (Bits store)
         => (store -> elem) -> store -> Int -> elem
peekFromBits reader store offset =
    reader $ store `shiftR` offset

instance PureStaticStorable Word64 Bool where
    poke store offset x  = pokeFromBits (\case {True -> setBit 0 zeroBits; False -> zeroBits}) store offset x
    peek = peekFromBits (`testBit` 0)

instance PureStaticStorable Word64 a => PureStaticStorable Word64 (Maybe a) where
    poke store offset Nothing = clearBit store offset
    poke store offset (Just x) =
        poke (setBit store offset) (offset+1) x

    peek store offset =
        case testBit store offset of
            False -> Nothing
            True -> Just $ peek store (offset +1)

instance (PureStaticStorable Word64 a, PureStaticStorable Word64 b) => PureStaticStorable Word64 (Either a b) where
    poke store offset val =
        case val of
            Left x -> poke (clearBit store offset) (offset+1) x
            Right x -> poke (setBit store offset) (offset+1) x

    peek store offset =
        case testBit store offset of
            False -> Left $ peek store (offset+1)
            True  -> Right $ peek store (offset+1)



------------------------

newtype PackedField (flds :: [(Symbol,Type)]) store = PackedField store

pokePackedField :: forall a (f :: Symbol) flds store.
                   (PureStaticStorable store a, KnownNat (PackedFieldOffset' f flds 0)
                   ,PackedFieldType f flds ~ a)
                => Proxy f -> a -> PackedField flds store -> PackedField flds store
pokePackedField _ x (PackedField store) =
    let idx = fromIntegral $ natVal' (proxy# :: Proxy# (PackedFieldOffset f flds)) :: Int
    in PackedField (poke store idx x)

peekPackedField :: forall a (f :: Symbol) flds store.
                   (PureStaticStorable store a, KnownNat (PackedFieldOffset' f flds 0)
                   ,PackedFieldType f flds ~ a)
                => Proxy f -> PackedField flds store -> a
peekPackedField _ (PackedField store) =
    let idx = fromIntegral $ natVal' (proxy# :: Proxy# (PackedFieldOffset f flds)) :: Int
    in (peek store idx)

newPackedField :: Num store => PackedField flds store
newPackedField = PackedField 0




