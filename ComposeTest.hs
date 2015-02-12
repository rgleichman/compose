import Compose

-- call "unwrap" on these values to get a result
-- "toSame" can be used to show a wrapped value

add2 :: WSame (Integer -> Integer)
add2 = wrap (+2)

times3 :: WSame (Integer -> Integer)
times3 = wrap (*3)

show' :: Show a => W (a -> String)
show' = wrap (show)

i4 :: W Integer
i4 = wrap 4

i3 :: W Integer
i3 = wrap 3

hello :: W String
hello = wrap "Hello"

addTimes :: WSame (Integer -> Integer)
addTimes = add2 `composeFunc` times3

showAddTimes :: WSame (Integer -> String)
showAddTimes = show' `composeFunc` addTimes

helloi4 :: Wrapped ((String -> Integer -> c) -> c)
helloi4 = hello `composeArgs` i4

i3helloi4 :: Wrapped ((Integer -> String -> Integer -> c) -> c)
i3helloi4 = i3 `composeArgs` helloi4

world5 :: WSame (String, Integer)
world5 = apply (wrap (\x y -> (x ++ " world", y + 1))) helloi4

fourWorld5 :: WSame (Integer, String, Integer, String)
fourWorld5 = apply (wrap (\z x y w -> (z, x ++ " world", y + 1, w))) (i3 `composeArgs` helloi4 `composeArgs` hello)

showAddTimes4 :: WSame String
showAddTimes4 = apply showAddTimes i4

times3Add2To4 :: WSame Integer
times3Add2To4 = apply times3 $ apply add2 i4

doubleCompose :: WSame (Integer)
doubleCompose = apply ((wrap (+)) `composeFunc` (wrap (+2))) ((wrap 3) `composeArgs` (wrap 4))
