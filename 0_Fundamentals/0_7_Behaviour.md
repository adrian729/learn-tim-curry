
## Immutability
Values are **never** changed. They can be assigned only **once**.

How to do things then?
**RECURSION**. No loops!

```
cont :: Int -> [Int] -> Int
count n list = go 0 list
    where
        go :: Int -> [Int] -> Int
        go result l =
            if null l   -- if the list is empty..
            then result -- ..then return our accumulated result
            else if head l == n
                then go (result + 1) (tail l)
                else go result       (tail l)
```

## Lambda functions
Functions without a name
```
\var1 var2 ... varN -> expr
```

Example, having:
```
satisfies :: (Int -> Bool) -> Int -> String
satisfies check n
    | check n = "The number " ++ show n ++ " passes check"
    | otherwise = "The number " ++ show n ++ " doesn't pass"
```
calling with a lambda:
```
ghci> satisfies (\x -> x > 0) 5
"The number 5 passes check"
ghci> satisfies (\x -> x > 0) (-3)
"The number -3 doesn't pass"
```

## Partial application
Applying only a part of the parameters to a function returns another function (expecting the missing params):
```
ghci> :t div
div :: Integral a => a -> a -> a
ghci> :t +d div
div :: Integer -> Integer -> Integer
ghci> div12By = div 12
ghci> :t div12By
div12By :: Integral a => a -> a
ghci> :t +d div12By
div12By :: Integer -> Integer
ghci> div12By 3
4
```

Applying a number to div, which expects two, returns a function that expects the missing number.

Another example:
```
applyTwice :: (Integer -> Integer) -> Integer -> Integer
applyTwice f x = f (f x)
```

```
ghci> applyTwice (+ 20) 17
57
ghci> applyTwice (* 2) 4
16
```

## Standard HOFs
Example of standard HOFs: map, filter, any, concatMap, iterate

```
ghci> map not [True, False, True]
[False,True,False]
ghci> map (* 2) [1 .. 5]
[2,4,6,8,10]
ghci> filter even [3, 1, 2, 5, 6, 8]
[2,6,8]
ghci> filter (<3) [10, 9 .. 0]
[2,1,0]
ghci> any (> 10) [2, 5, 7, 9]
False
ghci> concatMap (replicate 3) [1 .. 5]
[1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]
ghci> take 10 (iterate (* 2) 1)
[1,2,4,8,16,32,64,128,256,512]
```

## Functions inside lists
Everything is a function! So of course there can be lists of functions or partials.

```
ghci> head [(* 2), div 12, (+ 10)] 5
10
ghci> last [(* 2), div 12, (+ 10)] 5
15
```