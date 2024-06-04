module Example where

increase :: Integer -> Integer
increase x = x + 1

satisfies :: (Int -> Bool) -> Int -> String
satisfies check n
  | check n = "The number " ++ show n ++ " passes check"
  | otherwise = "The number " ++ show n ++ " doesn't pass"

applyTwice :: (Integer -> Integer) -> Integer -> Integer
applyTwice f x = f (f x)
