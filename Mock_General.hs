{-# LANGUAGE RankNTypes, FlexibleInstances, NoMonomorphismRestriction , MultiParamTypeClasses, FlexibleContexts, IncoherentInstances, ConstraintKinds, AllowAmbiguousTypes #-}
module Mock (apply
            ) where

--TODO: wrap all the values
class App a b c where
  apply :: a -> b -> c
--  apply f x = f x

type NApp a b = App (a-> b) a b

--Remember to put the App constraint in the type, or else it will use the default instance
infixl 9 @@
(@@) :: App (a->b) c d=> (a -> b) -> c -> d
f @@ x = apply f x

infixr 0 $$
($$) :: App (a->b) c d=> (a -> b) -> c -> d
f $$ x = apply f x

instance App (a->b) a b where
  apply f x = f x

data Method = Negate | Square | PowerOfTwo

parentObject :: Method -> Integer -> Integer
parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

--testMethod :: (Wrapable (Integer -> Method -> Integer) (Integer -> Method -> Integer), Wrapable b b, Wrapable (Method -> b) (Method -> b), Wrapable Method b) => Wrapped ((Integer -> Method -> Integer) -> Method -> b) -> [b]
--testMethod :: Wrapped ((Integer -> Method -> Integer) -> Method -> b) -> [b]
--testMethod :: Integer -> String
--TODO generalize the type signiture (possible add scoped type varibles) to allow foo to only square once when given an (ApplyOnce). Might also try adding a functional dependency to App typeclass, or make all data wrapped
testMethod :: (App (Integer -> String) c String, App (Integer -> Integer) c Integer) => c -> String
testMethod x = showX ++ ((show :: [Integer] -> String) $ [neg, sqr, two, foo])
  where
    [neg, sqr, two] = fmap (\method -> ((parentObject @@ method)::Integer->Integer) @@ x) methods
    foo = ((parentObject @@ Square) :: Integer -> Integer) @@ (two::Integer)
    showX = apply (show:: Integer -> String) x
    methods = [Negate, Square, PowerOfTwo]

testWrapAddOne x = testMethod x--(wrapAddOne x)
testTestMethod x = testMethod x

--instance Wrapable a b where
--  wrap x = Wrap ($ x)

instance Num a => App (Method-> (a->a)) Method (a -> a) where
  apply f m = case m of
    Negate -> (\x -> (f Square x) - 10)
    _ -> f m

newtype ApplyOnce a = ApplyOnce a deriving (Show)

instance App (a->a) (ApplyOnce a) (ApplyOnce a) where
  apply f (ApplyOnce a) = ApplyOnce $ f a

instance App (a->b) (ApplyOnce a) b where
  apply f (ApplyOnce a) = f a

--Apply twice
instance App (a->a) a a where
  apply f a = f $ f a

--instance (Num c) => App (Method -> c) a where
--  apply f x = 

--applyTwice :: (Show s, Num s) => s -> String

--applyTwice x = (show::Integer->String) @@ x ++ ((show::Integer->String) $ ((+1)::Integer->Integer) @@ x)
--applyTwice x = apply show x

--applyTwice x = show @@ x ++ (show $ (+1) @@ x)

applyTwice x = (show::Integer->String) @@ x ++ ((show::Integer->String) $ ((+1)::Num a=>a->a) @@ x)
