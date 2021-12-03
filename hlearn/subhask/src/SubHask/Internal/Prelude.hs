module SubHask.Internal.Prelude
    (
    -- * classes
    Show (..)
    , Read (..)
    , read

    , Storable (..)

    -- * data types
    , String
    , FilePath
    , Char
    , Int
    , Int8
    , Int16
    , Int32
    , Int64
    , Integer
    , Float
    , Double
    , Rational
    , Bool (..)

    , IO
    , ST
    , Maybe (..)
    , Either (..)

    -- * Prelude functions
    , build
    , (++)

    , Prelude.all
    , map

    , asTypeOf
    , undefined
    , otherwise
    , error
    , seq

    -- * subhask functions
    , assert
    , ifThenElse

    -- * Modules
    , module Control.DeepSeq
    , module Data.Proxy
    , module Data.Typeable
    , module GHC.TypeLits

    -- * Non-Prelude types

    -- ** QuickCheck
    , Arbitrary (..)
    , CoArbitrary (..)
    , coarbitraryShow

    -- * Extensions
    , Constraint
    )
    where

import Control.DeepSeq
import Control.Monad.ST
import Data.Foldable
import Data.List (foldl, foldl', foldr, foldl1, foldl1', foldr1, map, (++), intersectBy, unionBy )
import Data.Maybe
import Data.Typeable
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import GHC.Exts
import GHC.Int
import Prelude
import Test.QuickCheck.Arbitrary
import Foreign.Storable

{-# INLINE ifThenElse #-}
-- ifThenElse a b c = if a then b else c
ifThenElse a b c = case a of
    True -> b
    False -> c

-- |
--
-- FIXME:
-- Move to a better spot
-- Add rewrite rules to remove with optimization -O
assert :: String -> Bool -> a -> a
assert str b = if b
    then id
    else error $ "ASSERT FAILED: "++str

