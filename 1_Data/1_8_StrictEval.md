# Strict evaluation

## Space leaks
Lazines can lead to space leaks
```Haskell
sum :: [Int] -> Int
sum go 0
    where
        go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (x : xs) = go (acc + x) xs
```

## Debugging in Haskell: Equational Reasoning
```Haskell
sum [3, 1, 2]
    = go 0 [3, 1, 2]
    = go (0 + 3) [1, 2]
    = go ((0 + 3) + 1) [2]
    = go (((0 + 3) + 1) + 2) []
    = ((0 + 3) + 1) + 2
    = (3 + 1) + 2
    = 4 + 2
    = 6
```
The problem is that since Haskell is lazy, the addition is not evaluated until the end, so the stack grows with each addition. This is a space leak.

## BangPatterns
```Haskell
{-# LANGUAGE BangPatterns #-}
```

Adding the **BangPatterns** pragma allows forcing the evaluation of lazy computations using bangs `!`, to some degree.

```Haskell
sum [3, 1, 2]
    = go 0 [3, 1, 2]
    = go 3 [1, 2]
    = go 4 [2]
    = go 6 []
    = 6
```
No more space leaks!
