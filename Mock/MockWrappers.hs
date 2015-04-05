{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MockWrappers where

import Apply
import MockTags

--WrapDef
newtype WrapDef a = WrapDef a deriving (Show)

instance Wrap WrapDef where
  unwrap (WrapDef a) = a
  wrap = WrapDef

-- WrapMath
newtype WrapMath a = WrapMath a deriving (Show)

instance Wrap WrapMath where
  unwrap (WrapMath a) = a
  wrap = WrapMath

--WrapTwice
newtype WrapTwice a = WrapTwice a deriving (Show)

instance Wrap WrapTwice where
  wrap = WrapTwice
  unwrap (WrapTwice a) = a

instance App TAa_a WrapTwice a a where
  apply = makeApplyInst (\f -> f.f)

instance App TAb_b WrapTwice a a where
  apply = makeApplyInst (\f -> f.f.f)
