{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MockTests where

import MockWrappers
import MockTags
import Apply

--TODO: Investigate mocking mocks (without needing unwrapFunc) while keeping it easy to mock returned functions

--Method and WrapMath
data Method = Negate | Square | PowerOfTwo

instance TagC AMet_aa Method (a->a) where
  data TagD AMet_aa Method (a->a) = AMet_aa

instance Num a => App AMet_aa WrapMath Method (a -> a) where
  apply = makeApplyInst
          (\f m -> case m of
                    Negate -> (\x -> (f Square x) * (f Negate x + 1))
                    _ -> f m)

parentObject :: Method -> Integer -> Integer
parentObject Negate x = -x
parentObject Square x = x^(2::Integer)
parentObject PowerOfTwo x = 2^(x::Integer)                  

testMethod :: (App TAa_a w Integer Integer, App AMet_aa w Method (Integer -> Integer)) => w Integer -> String
testMethod x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap (\method -> wApply2 AMet_aa Aa_a parentObject (wrap method) x) methods
    showX = unwrap $ show #@ ADef & x
    methods = [Negate, Square, PowerOfTwo]

unwrapFunc :: Wrap w => (w a -> w b) -> w (a->b)
unwrapFunc f = wrap (\x -> unwrap $ f (wrap x))

testDoubleInherit :: (App TAa_a w Integer Integer, App AMet_aa w Method (Integer -> Integer)) => w Integer -> String
testDoubleInherit x = showX ++ show (fmap unwrap [neg, sqr, two])
  where
    [neg, sqr, two] = fmap func methods
    child1 :: (App AMet_aa w Method (Integer -> Integer)) => w Method -> w (Integer -> Integer)
    child1 = wApply parentObject AMet_aa
    child1Unwrapped = unwrapFunc child1
    --func method = wApply2 AMet_aa Aa_a parentObject (wrap method) x
    --func method = apply (child1 (wrap method)) Aa_a x
    --func method = apply (apply child1Unwrapped AMet_aa (wrap method)) Aa_a x
    --func method = applyW child1Unwrapped AMet_aa method @@ Aa_a  & x
    func method = child1Unwrapped @# AMet_aa & method @@ Aa_a  & x
    showX = unwrap $ show #@ ADef & x
    methods = [Negate, Square, PowerOfTwo]


applyTwice :: (App TAa_a w b b, Show b, Num b) => w b -> String
applyTwice x = unwrap (wApply show ADef x) ++ (show . unwrap $ wApply (+1) Aa_a x)

applyMultiple :: (App TAa_a w b b, App TAb_b w b b, Num b) => w b -> (w b, w b)
applyMultiple x = (wApply (+1) Aa_a x, wApply (+1) Ab_b x)

-- BAD STUFF --
--These should be compiler errors

--This should be an error
--applyTwiceBad x = unwrap (wApply Aa_a show x) ++ (show . unwrap $ wApply Aa_a (+1))

--This should also be an error, since there is no instance (TagC String Int)
--instance App TAa_a WrapTwice String Int where
--  apply (WrapTwice f) _ (WrapTwice a) = WrapTwice $ f a

testValues :: [String]
testValues = [
  "testMethod:"
  ,"(WrapDef 3)"
  ,show $ testMethod (WrapDef 3)
  ,"(WrapMath 3)"
  ,show $ testMethod (WrapMath 3)
  ,"(WrapTwice 3)"
  ,show $ testMethod (WrapTwice 3)
  ,"applyMultiple (WrapTwice 3)"
  ,show $ applyMultiple (WrapTwice 3)
   ]

main :: IO ()
main = mapM_ putStrLn testValues
