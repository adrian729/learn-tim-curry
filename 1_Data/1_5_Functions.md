# Functions

## $\eta$-reduction - Eta-reduction
$\forall x : f(x) \equiv g(x)$ means $f \equiv g$

```Haskell
showInt :: Int -> String
showInt n = show n
```

$\eta$-reduced:
```Haskell
showInt :: Int -> String
showInt = show
```

$\eta$-reduction allows avoiding the need to write the argument when defining an equivalent function.

### Examples

```Haskell
onlyEven :: [Int] -> [Int]
onlyEven xs = filter even xs
```

$\eta$-reduced:
```Haskell
onlyEven :: [Int] -> [Int]
onlyEven = filter even
```

```Haskell
prod :: [Int] -> [Int] -> [Int]
prod xs ys = zipWidth (*) xs ys
```

$\eta$-reduced:
```Haskell
prod :: [Int] -> [Int] -> [Int]
prod = zipWidth (*)
```

### When NOT to use $\eta$-reduction
If the variable names are not arbitrary and have some meaning, it may be better to not use $\eta$-reduction.

## Function composition
In Haskell the `.` operator is used for function composition.

```Haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
```
This example means composition of two functions, one `(a -> b)` and the other `(b -> c)`, returning a new function that is the composition of the two and will take an `a` and return a `c`.

### Examples

```Haskell
ghci> (length . show) True
4
ghci> map (length . words) ["Hi all", "Nice to meet you", "42"]
[2,4,1]
ghci> map (even . length . words) ["Hi all", "Nice to meet you", "42"]
[True,True,False]
```

### Composition with $\eta$-reduction
Having:
```Haskell
takeEven5 :: [[a]] -> [[a]]
takeEven5 list = take 5 (filter (\l -> even (length l)) list)
```

Applying composition:
```Haskell
takeEven5 :: [[a]] -> [[a]]
takeEven5 list = take 5 (filter (\l -> (even . length) l) list)
```

Applying $\eta$-reduction:
```Haskell
takeEven5 :: [[a]] -> [[a]]
takeEven5 list = take 5 (filter (even . length) list)
```

Applying again composition:
```Haskell
takeEven5 :: [[a]] -> [[a]]
takeEven5 = (take 5 . filter (even . length)) list
```

Applying $\eta$-reduction:
```Haskell
takeEven5 :: [[a]] -> [[a]]
takeEven5 = take 5 . filter (even . length)
```

By using composition and $\eta$-reduction several times, the code becomes more concise and readable in the end!
