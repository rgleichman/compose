{-# LANGUAGE RankNTypes, FlexibleInstances, NoMonomorphismRestriction , MultiParamTypeClasses, FlexibleContexts, IncoherentInstances #-}
module Mock (apply
            ) where

class App a b where
  apply :: (a -> b) -> a -> b
  apply f x = f x

--Remember to put the App constraint in the type, or else it will use the default instance
infixl 9 @@
(@@) :: App a b => (a -> b) -> a -> b
f @@ x = apply f x

infixr 0 $$
($$) :: App a b => (a -> b) -> a -> b
f $$ x = apply f x

instance App a b

data Method = Negate | Square | PowerOfTwo

parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

--testMethod :: (Wrapable (Integer -> Method -> Integer) (Integer -> Method -> Integer), Wrapable b b, Wrapable (Method -> b) (Method -> b), Wrapable Method b) => Wrapped ((Integer -> Method -> Integer) -> Method -> b) -> [b]
--testMethod :: Wrapped ((Integer -> Method -> Integer) -> Method -> b) -> [b]
testMethod :: Integer -> String
testMethod x = showX ++ (show $ [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> parentObject @@ method @@ x) methods
    showX = apply show x
    methods = [Negate, Square, PowerOfTwo]

--TODO: rewrite
{-
wrapAddOne :: Num a => t -> WFull t (Method -> a)
--wrapAddOne :: Num a => t -> Wrapped ((t -> Method -> a) -> Method -> a)
wrapAddOne x = Wrap childObjectFunction
  where
    childObjectFunction parentObj method = 
      case method of
       Negate -> (appliedParent method) * (appliedParent PowerOfTwo)
       _ -> appliedParent method
      where
        appliedParent = parentObj x
-}

testWrapAddOne x = testMethod x--(wrapAddOne x)
testTestMethod x = testMethod x

--instance Wrapable a b where
--  wrap x = Wrap ($ x)

instance Num a => App Method (a -> a) where
  apply f m = case m of
    Negate -> (\x -> (f Square x) - 10)
    _ -> f m

--Apply twice
instance App a a where
  apply f a = f $ f a

--instance (Num c) => App (Method -> c) a where
--  apply f x = 

applyTwice :: (Show s, Num s) => s -> String
applyTwice x = show @@ x ++ (show $ (+1) @@ x)
--applyTwice x = apply show x
