{-# LANGUAGE RankNTypes, FlexibleInstances #-}
module Compose (Wrapped, WFull, WSame, W, wrap, unwrap, apply, composeArgs, composeFunc, toSame) where

newtype Wrapped a = Wrap a

instance (Show a) => Show (WSame a) where
  show x = show (unwrap x)

type WFull a b = Wrapped ((a -> b) -> b)
type WSame a = WFull a a
type W a = forall b . WFull a b

wrap :: a -> WFull a b
wrap x = Wrap ($ x)

unwrap :: WSame a -> a
unwrap (Wrap x) = x id

apply :: WSame a -> Wrapped (a -> b) -> WFull b c
apply f (Wrap x) = wrap $ x (unwrap f)

composeArgs :: Wrapped (a -> b) -> Wrapped (b -> c) -> Wrapped (a -> c)
(Wrap x) `composeArgs` (Wrap y) = Wrap (y.x)

composeFunc :: WSame (b -> c) -> WSame (a -> b) -> WFull (a -> c) d
composeFunc f g = wrap $ unwrap f . unwrap g

toSame :: WSame a -> WSame a
toSame = id
