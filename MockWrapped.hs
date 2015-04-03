{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction , MultiParamTypeClasses, DeriveFunctor #-}
module MockWrapped (apply
                   ,(@@)
                   ,(!@)
                   ,(!@!)
                   ,(@!)
            ) where

class Wrappable w where
  unwrap :: w a -> a
  wrap :: a -> w a

class (Wrappable w) => App w a b where
  apply :: w (a -> b) -> w a -> w b

--Remember to put the App constraint in the type, or else it will use the default instance
infixl 9 @@
(@@) :: App w a b => w (a -> b) -> w a -> w b
f @@ x = apply f x

--Wraps the fuction
infixl 9 !@
(!@) :: App w a b => (a -> b) -> w a -> w b
f !@ x = apply (wrap f) x

--Wraps the fuction and argument
infixl 9 !@!
(!@!) :: App w a b => (a -> b) -> a -> w b
f !@! x = apply (wrap f) (wrap x)

--Wraps the argument
infixl 9 @!
(@!) :: App w a b => w (a -> b) -> a -> w b
f @! x = apply f (wrap x)


--TODO why is this instance picked when there is a more specific instance?
--instance (Functor w, Unwrappable w) => App w a b where
--  apply = fmap

data Method = Negate | Square | PowerOfTwo

parentObject :: Method -> Integer -> Integer
parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

testMethod' x = unwrap val
  where
    appliedParent = parentObject !@! Negate
    val = appliedParent @@ x

--testMethod :: (App w Method (Integer -> Integer), App w Integer String, App w Integer Integer) => w Integer -> String
--testMethod :: (Wrappable w, Applicative w) => w Integer -> String

{-
testMethod :: (App w Method (Integer -> Integer), App w Integer String, App w Integer Integer) => w Integer -> String
testMethod x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> parentObject !@! method @@ x) methods
    --showX = unwrap $ show !@ x
    --showX = unwrap $ apply (wrap show) x
    showX = "hello"
    methods = [Negate, Square, PowerOfTwo]
-}

--testMethodAp :: (Wrappable w, Applicative w, Show a) => w a -> String
testMethodAp' :: (App w a String, Show a) => w a -> String
testMethodAp' x = unwrap $ show !@ x

--testMethodAp :: (Wrappable w, Applicative w, Show a) => w a -> String
testMethodAp :: (App w a String, Show a) => w a -> String
testMethodAp x = showX
  where
    showX = unwrap $ show !@ x

--testWrapAddOne x = testMethod x--(wrapAddOne x)
--testTestMethod x = testMethod x

--instance Wrapable a b where
--  wrap x = Wrap ($ x)

--WrapDef

newtype WrapDef a = WrapDef a

instance Wrappable WrapDef where
  unwrap (WrapDef a) = a
  wrap = WrapDef

instance App WrapDef a b where
  apply (WrapDef f) (WrapDef a) = WrapDef $ f a

-- WrapMath
newtype WrapMath a = WrapMath a

instance Wrappable WrapMath where
  unwrap (WrapMath a) = a
  wrap = WrapMath

instance App WrapMath a b where
  apply (WrapMath f) (WrapMath a) = WrapMath $ f a

instance Num a => App WrapMath Method (a -> a) where
  apply (WrapMath f) (WrapMath m) = WrapMath $ case m of
    Negate -> (\x -> (f Square x) - 10)
    _ -> f m


--Apply twice
newtype WrapTwice a = WrapTwice a deriving (Functor)

instance Wrappable WrapTwice where
  wrap = WrapTwice
  unwrap (WrapTwice a) = a

instance App WrapTwice a b where
  apply (WrapTwice f) (WrapTwice a) = WrapTwice $ f a

instance App WrapTwice a a where
  apply (WrapTwice f) (WrapTwice a) = WrapTwice $ f $ f a


applyTwice :: (App w b String, App w b b, Show b, Num b) => w b -> String
applyTwice x = unwrap (show !@ x) ++ (show . unwrap $ (+1) !@ x)
