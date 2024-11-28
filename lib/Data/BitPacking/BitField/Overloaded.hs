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
{-# LANGUAGE UnliftedNewtypes #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use fewer LANGUAGE pragmas" #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}


module Data.BitPacking.BitField.Overloaded where

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

newtype BitField (fs :: [Symbol]) a = BitField { unBits :: a }
    deriving (Eq, Bits)


{-
In my mind the interface of the primops could be far simpler and safer *if* we don't care about
constructing *arbitrary* heap objects. And I'm not sure there is a good use case for doing so.

Under this assumption instead of dealing with heaps of Addr# values we can represent a hollow value
by a pointer, and a record of what it's pointing at. E.g. something like:

`newtype Hollow (lifted_con :: k2) (a :: TYPE k) = Hollow a`

Where lifted_con is always a constructor of type `a`. I'm not sure if there good way to express this
constraint. From that we could define the interface as:
-}

-- Add a way to represent "unfinished" values in the types
-- can and probably should be opaque to the user
newtype Hollow (lifted_con :: k2) (a :: TYPE k) = Hollow a

-- A way to insert/allocate a hollow constructor into a compact region.
compactAddHollowCon#
  :: forall r k lifted_con.
    Compact#         -- ^ the compact region where the constructor heap object will be added
  -> Proxy (lifted_con :: k)
  -> State# RealWorld -- ^ state token
  -> (# State# RealWorld
      , Hollow lifted_con r     -- ^ The allocated (unfinished) constructor heap object,
     #)
compactAddHollowCon# = undefined

-- A way to fill a hollow constructor.
fillHollowCon#
    :: Compact#
    -> Hollow lifted_con (a :: TYPE r)
    -- When used the arg_reps will be concrete, eg. [LiftedRep, IntRep] if there is a pointer and Int# field.
    -> (args :: TYPE (TupleRep arg_reps))
    -> State# RealWorld
    -> (# State# RealWorld, a #)
fillHollowCon# = undefined

-- To put it all into action:
example :: Compact# -> State# RealWorld -> (# State# RealWorld, Either () () #)
example compactRegion s0 =
   case compactAddHollowCon# compactRegion (Proxy @(Left '() )) s0 of
        (# s1, hollowLeft #) -> case compactAdd# compactRegion () s1 of -- We could use addAllow here, but since () has no fields it's pointless
            (# s2, compact_unit_value #) -> fillHollowCon# compactRegion hollowLeft (# compact_unit_value #) s2

-- Or slightly more complex:
example2 :: Compact# -> State# RealWorld -> (# State# RealWorld, (Maybe Int, ()) #)
example2 compactRegion s0 =
   case compactAddHollowCon# compactRegion (Proxy @( '(Maybe Int, ()) )) s0 of
        (# s1, hollowTuple #) -> case compactAddHollowCon# compactRegion (Proxy @(Just Int) ) s1 of
            (# s2, hollowJust #) -> case compactAdd# compactRegion (42 :: Int) s2 of
                (# s3, compactInt #) -> case fillHollowCon# compactRegion hollowJust (# compactInt #) s3 of
                    (# s4, filledJust #) -> case compactAdd# compactRegion () s4 of
                        (# s5, compactUnit #) -> fillHollowCon# compactRegion hollowTuple (# filledJust, compactUnit #) s5


-- TupleRep [LiftedRep, IntRep]

{-# INLINE setField #-}
setField :: forall a fs f.
         ( Bits a, KnownNat (ListIndex fs f 0) )
         => Proxy f -> BitField fs a -> BitField fs a
setField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in BitField $ setBit (unBits w) idx

{-# INLINE clearField #-}
clearField :: forall a fs (f::Symbol).
            ( Bits a, KnownNat (ListIndex fs f 0) )
            => Proxy f -> BitField fs a -> BitField fs a
clearField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in BitField $ clearBit (unBits w) idx

{-# INLINE getField #-}
getField :: forall a fs (f::Symbol).
         ( Bits a, KnownNat (ListIndex fs f 0) )
         => Proxy f -> BitField fs a -> Bool
getField _ w =
    let idx = fromIntegral $ natVal' (proxy# @(ListIndex fs f 0))
    in testBit w idx

newBitField :: forall a fs. Bits a => BitField fs a
newBitField = BitField (zeroBits :: a)

