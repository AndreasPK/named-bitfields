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
{-# LANGUAGE TypeApplications #-}

module Data.Examples.Array where

import Data.BitPacking.PackedFieldsM

import Data.Word
import Data.Array.Byte (MutableByteArray)
import Control.Monad.Primitive (RealWorld)

type MyArrayHeader = PackedFieldM IO ['("MagicNumber",Word8),'("Version", Word8),'("Nr",Word8)] (MutableByteArray RealWorld)

emptyHeader :: IO MyArrayHeader
emptyHeader = newPackedFieldM

