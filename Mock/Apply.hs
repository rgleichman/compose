{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Apply
       (App(..)
       ,Wrap(..)
       ,TagC(..)
       ,TagD(..)
       ,TDef
       ,makeApplyInst
       ,wApply
       ,applyW
       ,wApplyW
       ,wApply2
       ,(@@)
       ,(#@)
       ,(#@#)
       ,(@#)
       ,(&)
       ) where

class Wrap w where
  unwrap :: w a -> a
  wrap :: a -> w a

class TagC t a b where
  data family TagD t a b

class (Wrap w, TagC t a b) => App t w a b where
  apply :: w (a -> b) -> TagD t a b -> w a -> w b

makeApplyInst :: (Wrap w) => (a -> a1 -> s) -> w a -> t -> w a1 -> w s
makeApplyInst g f _ x = wrap $ g (unwrap f) (unwrap x)

wApply :: App t w a b => (a -> b) -> TagD t a b -> w a -> w b
wApply f = apply (wrap f)

applyW :: App t w a b => w (a -> b) -> TagD t a b -> a -> w b
applyW f t x= apply f t (wrap x)

wApplyW :: App t w a b => (a -> b) -> TagD t a b -> a -> w b
wApplyW f t x = apply (wrap f) t (wrap x)


--The types for the infix functions are not inferred correctly
infixl 9 @@
(@@) :: App t w a b => w (a -> b) -> TagD t a b -> w a -> w b
(@@) = apply

--Wraps the fuction
infixl 9 #@
(#@) :: App t w a b => (a -> b) -> TagD t a b -> w a -> w b
(#@) = wApply

--Wraps the fuction and argument
infixl 9 #@#
(#@#) :: App t w a b => (a -> b) -> TagD t a b -> a -> w b
(#@#) = wApplyW 

--Wraps the argument
infixl 9 @#
(@#) :: App t w a b => w (a -> b) -> TagD t a b -> a -> w b
(@#) = applyW

-- Usage: apply f t x == f @@ t & x
infixl 9 &
(&) :: (a -> b) -> a -> b
(&) = ($)

--Apply unwraped function to two wrapped arguments
wApply2 :: (App t1 w a1 (a -> b), App t w a b) => TagD t1 a1 (a -> b) -> TagD t a b -> (a1 -> a -> b) -> w a1 -> w a -> w b
wApply2 t1 t2 f = apply2 t1 t2 (wrap f)

--Apply wrapped function to two wrapped arguments
apply2 :: (App t1 w a1 (a -> b), App t w a b) => TagD t1 a1 (a -> b) -> TagD t a b -> w (a1 -> a -> b) -> w a1 -> w a -> w b
apply2 t1 t2 f x y= f @@ t1 & x @@ t2 & y

--Default tag
data TDef

instance TagC TDef a b where
  data TagD TDef a b = ADef

--If there is no instance for a tag and wrapper, use the default wrapper
instance (Wrap w, TagC t a b) => App t w a b where
  apply f _ = apply f ADef

--The default wrapper is normal function application
instance (Wrap w) => App TDef w a b where
  apply = makeApplyInst ($)
