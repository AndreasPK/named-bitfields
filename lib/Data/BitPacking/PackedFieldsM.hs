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

module Data.BitPacking.PackedFieldsM where

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
import Data.Primitive.ByteArray

import Data.BitPacking.TypeMachinery
import Data.Primitive (MutableArray)


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

-- | A way to store things in other things.
--
-- All offsets and sizes are in *bits*
--
--
--
class Monad m => StaticStorable m store elem where
    poke :: store -> Int -> elem -> m store
    peek :: store -> Int -> m elem

instance StaticStorable IO (MutableByteArray RealWorld) Bool where
    poke arr offset x = do
        let !byte_offset = (offset `div` 8)
        let !bit_offset = (offset `mod` 8)
        !(b :: Word8) <- readByteArray arr (offset `div` 8)
        let shifted_val = ((fromIntegral $ fromEnum x) `shiftL` bit_offset) :: Word8
            mask = complement (1 `shiftL` bit_offset) :: Word8
        let !b' = b .&. (mask .|. shifted_val)
        writeByteArray arr byte_offset b'
        return arr

    peek arr offset = do
        !(b :: Word8) <- readByteArray arr (offset `div` 8)
        return $ testBit b (offset `mod` 8)

instance StaticStorable IO (MutableByteArray RealWorld) Word8 where
    poke arr offset x = do
        let !byte_offset = (offset `div` 8)
        let !bit_offset = (offset `mod` 8)
        !(b :: Word8) <- readByteArray arr (offset `div` 8)
        let shifted_val = ((fromIntegral $ fromEnum x) `shiftL` bit_offset) :: Word8
            mask = complement (1 `shiftL` bit_offset) :: Word8
        let !b' = b .&. (mask .|. shifted_val)
        writeByteArray arr byte_offset b'
        return arr

    peek arr offset = do
        let !bit_offset = (offset `mod` 8)
            !byte_offset = (offset `div` 8)
        if bit_offset == 0
            then
                readByteArray arr byte_offset
            else do
                b1 <- readByteArray arr byte_offset
                b2 <- readByteArray arr (byte_offset + 1)
                return $ (b1 `shiftL` bit_offset) .&. (b2 `shiftR` (8-bit_offset))

newtype PackedFieldM m (flds :: [(Symbol,Type)]) store = PackedField store

pokePackedField :: forall a (f :: Symbol) flds store.
                   (StaticStorable IO store a, KnownNat (PackedFieldOffset' f flds 0)
                   ,PackedFieldType f flds ~ a)
                => Proxy f -> a -> PackedFieldM IO flds store -> IO (PackedFieldM IO flds store)
pokePackedField _ x (PackedField store) =
    let idx = fromIntegral $ natVal' (proxy# :: Proxy# (PackedFieldOffset f flds)) :: Int
    in PackedField <$> (poke store idx x)

peekPackedField :: forall a (f :: Symbol) flds store.
                   (StaticStorable IO store a, KnownNat (PackedFieldOffset' f flds 0)
                   ,PackedFieldType f flds ~ a)
                => Proxy f -> PackedFieldM IO flds store -> IO a
peekPackedField _ (PackedField store) =
    let idx = fromIntegral $ natVal' (proxy# :: Proxy# (PackedFieldOffset f flds)) :: Int
    in (peek store idx)

newPackedFieldM :: forall flds. (KnownNat (PackedFieldsSize flds)) => IO (PackedFieldM IO flds (MutableByteArray RealWorld))
newPackedFieldM =
    let sz = fromIntegral $ natVal' (proxy# :: Proxy# (PackedFieldsSize flds))
    in PackedField <$> newByteArray (1 + ((sz-1) `div` 8) )




