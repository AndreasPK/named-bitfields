{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS_GHC -dumpdir dumps -ddump-to-file -ddump-stg-final -ddump-cmm -ddump-simpl
    -ddump-ds -dshow-passes #-}


module Main where

import Data.Word

import Data.BitPacking.BitField
import Data.BitPacking.PackedFields
import Data.Proxy
import Data.Kind
import GHC.TypeLits
import GHC.Exts





type Attendants = BitField ["Hannes", "Jana", "Benny", "Samuel", "Chris", "&Co"]

-- newtype Foo (foo :: [ (Symbol,Type) ] ) = AAAA Int
-- type Bar = Foo '[ '("a", Bool) ]

type MyPackedField = PackedField '[ '("Foo", Word8), '("Bar", Maybe Bool), '("Baz", Either Bool Word8), '("Bam", Word8)] Word64

type MyPackedField2 = PackedField
    '[ '("Foo", Word8),
       '("Bar", Maybe Bool),
       '("Baz", Either Bool Word8),
       '("Bam", Bool),
       '("Bam2", Bool),
       '("Bam3", Bool),
       '("Bam4", Bool),
       '("Bam5", Bool),
       '("Bam6", Bool),
       '("Bam7", Bool),
       '("Bam8", Bool),
       '("Bam9", Bool),
       '("Bam10", Bool),
       '("Bam11", Bool),
       '("Bam12", Bool),
       '("Bam13", Bool),
       '("Bam14", Bool),
       '("Bam15", Bool),
       '("Bam16", Bool),
       '("Bam17", Bool),
       '("Bam18", Bool),
       '("Bam19", Bool),
       '("Bam20", Bool),
       '("Bam21", Bool),
       '("Bam22", Bool),
       '("Bam23", Bool),
       '("Bam24", Bool),
       '("Bam25", Bool),
       '("Bam26", Bool),
       '("Bam27", Bool),
       '("Bam28", Bool),
       '("Bam29", Bool)
       ]
     Word64

foo = newPackedField :: MyPackedField
bar = newPackedField :: MyPackedField2

adjustPacked2 :: MyPackedField2 -> MyPackedField2
adjustPacked2 x =  pokePackedField (Proxy @"Bam26") True $
            pokePackedField (Proxy @"Bam29") True $
            pokePackedField (Proxy @"Bam28") True $
            pokePackedField (Proxy @"Bam27") True $
            (x :: MyPackedField2)

mkPacked :: PackedField   ['("Foo", Word8), '("Bar", Maybe Bool),    '("Baz", Either Bool Word8), '("Bam", Word8)]   Word64
mkPacked =  pokePackedField (Proxy @"Foo") (0) $
            pokePackedField (Proxy @"Bar") (Just True) $
            pokePackedField (Proxy @"Baz") (Right 13) $
            pokePackedField (Proxy @"Bam") (254) $
            (newPackedField :: MyPackedField)

adjustPacked :: MyPackedField -> MyPackedField
adjustPacked x =  pokePackedField (Proxy @"Foo") (0) $
            pokePackedField (Proxy @"Bar") (Just True) $
            pokePackedField (Proxy @"Baz") (Right 13) $
            pokePackedField (Proxy @"Bam") (254) $
            (x :: MyPackedField)

main :: IO ()
main = do
    attendants <- pure $ setField (Proxy @"Hannes") . setField (Proxy @"Jana") $ (newBitField :: Attendants)
    print $ getField (Proxy @"Hannes") attendants
    print $ getField (Proxy @"Jana") attendants
    print $ getField (Proxy @"Benny") attendants
    print $ getField (Proxy @"Samuel") attendants




    packed <- pure $ noinline mkPacked
    print "PackedFields:"

    print $ (peekPackedField (Proxy @"Foo") packed)
    print $ (peekPackedField (Proxy @"Bar") packed)
    print $ (peekPackedField (Proxy @"Baz") packed)
    print $ (peekPackedField (Proxy @"Bam") packed)

    return ()

