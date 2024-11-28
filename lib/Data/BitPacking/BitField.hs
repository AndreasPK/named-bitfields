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

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use fewer LANGUAGE pragmas" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeApplications #-}

module Data.BitPacking.BitField where

import Data.Bits as Bits
import GHC.Exts
import GHC.Int
import Data.Kind
import GHC.Exts
import GHC.TypeError
import GHC.TypeLits
import Data.Proxy

import GHC.Word

import Data.BitPacking.TypeMachinery

newtype BitField (fs :: [Symbol]) = BitField { unBits :: Word64 }
    deriving (Eq, Bits)

{-# INLINE setField #-}
setField :: forall fs f. ( KnownNat (ListIndex fs f 0) )
         => Proxy f -> BitField fs -> BitField fs
setField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in BitField $ setBit (unBits w) idx

{-# INLINE clearField #-}
clearField :: forall fs (f::Symbol).
            ( KnownNat (ListIndex fs f 0) )
         => Proxy f -> BitField fs -> BitField fs
clearField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in BitField $ clearBit (unBits w) idx

{-# INLINE getField #-}
getField :: forall fs (f::Symbol).
            ( KnownNat (ListIndex fs f 0) )
         => Proxy f -> BitField fs -> Bool
getField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in testBit w idx


newBitField :: BitField a
newBitField = BitField 0

