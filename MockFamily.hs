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

--TODO: seperate modules
class Wrappable w where
  unwrap :: w a -> a
  wrap :: a -> w a

class Match t a b where
  data family Mock t a b

--class (Wrappable w, Match t a b) => App t w a b where
class (Wrappable w, Match t a b) => App t w a b where
  apply :: w (a -> b) -> Mock t a b -> w a -> w b

--This was commented out since including it sometimes caused the inferred type signitures to leave out App constraints
instance (Wrappable w, Match t a b) => App t w a b where
  apply f _ = apply f ADef

instance (Wrappable w) => App TDef w a b where
  apply f _ x = wrap $ (unwrap f) (unwrap x)

makeApplyInst g f _ x = wrap $ g (unwrap f) (unwrap x)

--instance (Wrappable w) => App ADef w a b where
--  apply _ f x = wrap $ (unwrap f) (unwrap x)

--wApply :: App t w a b => t -> (a -> b) -> w a -> w b
wApply f = apply (wrap f)

--applyW :: App t w a b => t -> w (a -> b) -> a -> w b
applyW f t x= apply f t (wrap x)

--wApplyW :: App t w a b => t -> (a -> b) -> a -> w b
wApplyW f t x = apply (wrap f) t (wrap x)

data TDef

instance Match TDef a b where
  data Mock TDef a b = ADef

--The types for the infix functions are not inferred
infixl 9 @@
(@@) :: App t w a b => w (a -> b) -> Mock t a b -> w a -> w b
(@@) = apply

--Wraps the fuction
infixl 9 #@
(#@) :: App t w a b => (a -> b) -> Mock t a b -> w a -> w b
(#@) = wApply

--Wraps the fuction and argument
infixl 9 #@#
(#@#) :: App t w a b => (a -> b) -> Mock t a b -> a -> w b
(#@#) = wApplyW 

--Wraps the argument
infixl 9 @#
(@#) :: App t w a b => w (a -> b) -> Mock t a b -> a -> w b
(@#) = applyW

infixl 9 &
(&) = ($)

--Apply unwraped function to two wrapped arguments
wApply2 :: (App t1 w a1 (a -> b), App t w a b) => Mock t1 a1 (a -> b) -> Mock t a b -> (a1 -> a -> b) -> w a1 -> w a -> w b
wApply2 t1 t2 f = apply2 t1 t2 (wrap f)

--Apply wrapped function to two wrapped arguments
apply2 :: (App t1 w a1 (a -> b), App t w a b) => Mock t1 a1 (a -> b) -> Mock t a b -> w (a1 -> a -> b) -> w a1 -> w a -> w b
apply2 t1 t2 f x y= f @@ t1 & x @@ t2 & y

data Method = Negate | Square | PowerOfTwo

parentObject :: Method -> Integer -> Integer
parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

testMethod :: (App TDef w Integer String, App TAa_a w Integer Integer, App AMet_aa w Method (Integer -> Integer)) => w Integer -> String
testMethod x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> wApply2 AMet_aa Aa_a parentObject (wrap method) x) methods
    showX = unwrap $ show #@ ADef & x
    methods = [Negate, Square, PowerOfTwo]


--testMethodAp' :: (Wrappable w, Show a) => w a -> String
testMethodAp' x = unwrap $ show #@ ADef & x

--testMethodAp :: (Wrappable w, Show a) => w a -> String
testMethodAp x = showX
  where
    showX = unwrap $ show #@ ADef & x

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
--  apply _ (WrapMath f) (WrapMath m) = WrapMath $ case m of
--    Negate -> (\x -> (f Square x) - 10)
--    _ -> f mn
  apply = makeApplyInst
          (\f m -> case m of
                    Negate -> (\x -> (f Square x) - 10)
                    _ -> f m)

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

instance App TAa_a WrapTwice a a where
  apply = makeApplyInst (\f -> f.f)

--instance (Match t a b) => App t WrapTwice a b where
--  apply = applyDef

--applyTwice :: (App Aa_a w b b, App ADef w b String, Show b, Num b) => w b -> String
applyTwice :: (App TDef w b String, App TAa_a w b b, Show b, Num b) => w b -> String
applyTwice x = unwrap (wApply show ADef x) ++ (show . unwrap $ wApply (+1) Aa_a x)

data TAb_b

instance Match TAb_b c c where
  data Mock TAb_b c c = Ab_b

instance App TAb_b WrapTwice a a where
  apply = makeApplyInst (\f -> f.f.f)

applyMultiple :: (App TAa_a w b b, App TAb_b w b b, Num b) => w b -> (w b, w b)
applyMultiple x = (wApply (+1) Aa_a x, wApply (+1) Ab_b x)


-- BAD STUFF --
--These should be compiler errors

--This should be an error
--applyTwiceBad x = unwrap (wApply Aa_a show x) ++ (show . unwrap $ wApply Aa_a (+1))

--This should also be an error, since there is no instance (Match String Int)
--instance App TAa_a WrapTwice String Int where
--  apply _ (WrapTwice f) (WrapTwice a) = WrapTwice $ f a
