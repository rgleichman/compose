{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module MockFamily
       (apply
       ,wApply
       ,applyW
       ,wApplyW
--       ,(@@)
--       ,(!@)
--       ,(!@!)
--       ,(@!)
       ) where

class Wrappable w where
  unwrap :: w a -> a
  wrap :: a -> w a

class Match t a b where
  data family Mock t a b

--class (Wrappable w, Match t a b) => App t w a b where
class (Wrappable w, Match t a b) => App t w a b where
  apply :: (Mock t a b) -> w (a -> b) -> w a -> w b

--This was commented out since including it sometimes caused the inferred type signitures to leave out App constraints
instance (Wrappable w, Match t a b) => App t w a b where
  apply = applyDef

applyDef :: (Wrappable w) => t -> w (a -> b) -> w a -> w b
applyDef _ f x = wrap $ (unwrap f) (unwrap x)

--instance (Wrappable w) => App ADef w a b where
--  apply _ f x = wrap $ (unwrap f) (unwrap x)

--wApply :: App t w a b => t -> (a -> b) -> w a -> w b
wApply t f x = apply t (wrap f) x

--applyW :: App t w a b => t -> w (a -> b) -> a -> w b
applyW t f x= apply t f (wrap x)

--wApplyW :: App t w a b => t -> (a -> b) -> a -> w b
wApplyW t f x = apply t (wrap f) (wrap x)

data TDef

instance Match TDef a b where
  data Mock TDef a b = ADef

{-
--Remember to put the App constraint in the type, or else it will use the default instance
infixl 9 @@
--(@@) :: App w a b => w (a -> b) -> w a -> w b
f @@ t = apply t f

--Wraps the fuction
infixl 9 !@
--(!@) :: App w a b => (a -> b) -> w a -> w b
f !@ t = apply t (wrap f)

--Wraps the fuction and argument
infixl 9 !@!
--(!@!) :: App w a b => (a -> b) -> a -> w b
f !@! t = (\x -> apply t (wrap f) (wrap x))

--Wraps the argument
infixl 9 @!
--(@!) :: App w a b => w (a -> b) -> a -> w b
f @! t = (\x -> apply t f (wrap x))
-}


data Method = Negate | Square | PowerOfTwo

parentObject :: Method -> Integer -> Integer
parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

{-
testMethod' x = unwrap val
  where
    appliedParent = parentObject !@! Negate
    val = appliedParent @@ x
-}

--testMethod :: (App w Method (Integer -> Integer), App w Integer String, App w Integer Integer) => w Integer -> String
--testMethod :: (Wrappable w, Applicative w) => w Integer -> String


--testMethod :: (App Aa_a w Integer Integer, App AMet_aa w Method (Integer -> Integer)) => w Integer -> String
--testMethod :: (App Aa_a w Integer Integer, App AMet_aa w Method (Integer -> Integer), App ADef w Integer String) => w Integer -> String
testMethod x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> apply Aa_a (wApplyW AMet_aa parentObject method) x) methods
    --showX = unwrap $ show !@ x
    showX = unwrap $ wApply ADef show x
    --showX = "hello"
    methods = [Negate, Square, PowerOfTwo]


--testMethodAp' :: (Wrappable w, Show a) => w a -> String
testMethodAp' x = unwrap $ apply ADef (wrap show) x

--testMethodAp :: (Wrappable w, Show a) => w a -> String
testMethodAp x = showX
  where
    showX = unwrap $ wApply ADef show x

--testWrapAddOne x = testMethod x--(wrapAddOne x)
--testTestMethod x = testMethod x

--instance Wrapable a b where
--  wrap x = Wrap ($ x)

--WrapDef

newtype WrapDef a = WrapDef a deriving (Show)

instance Wrappable WrapDef where
  unwrap (WrapDef a) = a
  wrap = WrapDef

--instance (Match t a b) => App t WrapDef a b where
--   apply = applyDef

--instance App AGen WrapDef a b where
--  apply _ (WrapDef f) (WrapDef a) = WrapDef $ f a

-- WrapMath
newtype WrapMath a = WrapMath a deriving (Show)

instance Wrappable WrapMath where
  unwrap (WrapMath a) = a
  wrap = WrapMath

--instance App AGen WrapMath a b where
--  apply (WrapMath f) (WrapMath a) = WrapMath $ f a

data AMet_aa

instance Match AMet_aa Method (a->a) where
  data Mock AMet_aa Method (a->a) = AMet_aa
--data AMet_aa :: * -> * -> * where
--  AMet_aa :: AMet_aa Method (c->c)

instance Num a => App AMet_aa WrapMath Method (a -> a) where
  apply _ (WrapMath f) (WrapMath m) = WrapMath $ case m of
    Negate -> (\x -> (f Square x) - 10)
    _ -> f m

instance App TDef WrapMath a b where
  apply = applyDef

instance App TAa_a WrapMath a a where
  apply = applyDef
--instance (Match t a b) => App t WrapMath a b where
--  apply = applyDef

--instance App t WrapMath a b where
--  apply = applyDef

--Apply twice
newtype WrapTwice a = WrapTwice a deriving (Show)

instance Wrappable WrapTwice where
  wrap = WrapTwice
  unwrap (WrapTwice a) = a

data TAa_a

instance Match TAa_a a a where
  data Mock TAa_a a a = Aa_a

{-
data family Aa_a a b
data instance Aa_a c c where
  Aa_a :: Aa_a c c
--data Aa_a :: * -> * -> * where
--  Aa_a :: Aa_a c c
-}

instance App TAa_a WrapTwice a a where
  apply _ (WrapTwice f) (WrapTwice a) = WrapTwice $ f $ f a

--instance (Match t a b) => App t WrapTwice a b where
--  apply = applyDef

--applyTwice :: (App Aa_a w b b, App ADef w b String, Show b, Num b) => w b -> String
--applyTwice :: (App TDef w b String, App TAa_a w b b, Show b, Num b) => w b -> String
applyTwice :: (App TDef w b String, App TAa_a w b b, Show b, Num b) => w b -> String
applyTwice x = unwrap (wApply ADef show x) ++ (show . unwrap $ wApply Aa_a (+1) x)


data TAb_b


instance Match TAb_b c c where
  data Mock TAb_b c c = Ab_b

--data Ab_b :: * -> * -> * where
--  Ab_b :: Ab_b c c

instance App TAb_b WrapTwice a a where
  apply _ (WrapTwice f) (WrapTwice a) = WrapTwice $ f $ f $ f a


--applyMultiple :: (App Ab_b w b b, App Aa_a w b b, Num b) => w b -> (w b, w b)
applyMultiple x = (wApply Aa_a (+1) x, wApply Ab_b (+1) x)


-- BAD STUFF --
--These should be compiler errors

--This should be an error
--applyTwiceBad x = unwrap (wApply Aa_a show x) ++ (show . unwrap $ wApply Aa_a (+1))

--This should also be an error, since there is no instance (Match String Int)
--instance App TAa_a WrapTwice String Int where
--  apply _ (WrapTwice f) (WrapTwice a) = WrapTwice $ f a
