# Pattern Matching

## Patterns
We will talk about patterns as specific *values* or *shapes* of input. It describes how an input looks like.

### case-of
You write the function once for each pattern. If your input matches one pattern, then the function will behave that way.

#### syntax
```Haskell
f :: type
f pattern1 = result1
f pattern2 = result2
...
f patternN = resultN
```

You can also use **case-of** to write the same
```Haskell
f :: type
f ... = case var of
    pattern1 -> result1
    pattern2 -> result2
    ...
    patternN -> resultN
```

#### examples
```Haskell
not :: Bool -> Bool
not True = False
not False = True
```

In this example, the variable *n* is used to mean any other value.
```Haskell
isZero :: Int -> Bool
isZero 0 = True
isZero n = False
```

The undescore pattern (*_*) is a pattern that matches anything.
```Haskell
eval :: Char -> Int -> Int -> Int
eval op x y = case op of
    '+' -> x + y
    '-' -> x - y
    '*' -> x * y
    '/' -> div x y
    _ -> 0
```

#### Patterns on lists
```Haskell
isEmpty :: [Int] -> Bool
isEmpty [] = True
isEmpty _ = False
```

```Haskell
-- given a list with exactly 3 values, sums the first and the last one. Otherwise, returns 0
sumOfTwoInThree :: [Int] -> Int
sumOfTwoInThree [x, _, y] = x + y
sumOfTwoInThree _         = 0
```

```Haskell
oneOrTwoZeroes :: [Int] -> Bool
oneOrTwoZeroes l = case l of
    [0]    -> True
    [0, 0] -> True
    _      -> False
```

### Structural List Patterns
- Empty list **[]**
- An element prepended to a list **x : xs**
  - **x** represents the head
  - **:** concat
  - **xs** represents the tail

Actually, array syntax is syntax sugar for structural list patterns
```Haskell
ghci> [3, 1, 2] == 3 : 1 : 2 : []
True
ghci> 3 : 1 : 2 : [] == 3 : (1 : (2 : []))
True
```
#### examples
```
-- check if a list has at least two elements
atLeastTwo :: [Int] -> Bool
atLeastTwo (x0 : x1 : xs) = True
atLeastTwo _ = False
```

```Haskell
headOrDef :: Int -> [Int] -> Int
headOrDef def [] = def
headOrDef _ (x : _) = x
```

```Haskell
dropHead :: [Int] -> [Int]
dropHead [] = []
dropHead (_ : xs) = xs
```

#### List Recursion with pattern matching
```Haskell
-- very slow implementation!
sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs
```

```Haskell
-- count how many times n appears in list
count :: Int -> [Int] -> Int
count n list = go 0 list
  where
    go :: Int -> [Int] -> Int
    go acc (x : xs)
      | x == n = go (acc + 1) xs
      | otherwise = go acc xs
```

#### Tip
**ALWAYS** use pattern matching on lists instead of unsafe **head** and **tail** functions whenever you need them.

#### What can go wrong?

##### Pattern order matters
Pattern matching will check the **FIRST** pattern that matches top to bottom. The following example will always return False since the **underscore pattern** matches any input!
```Haskell
isEmpty :: [Int] -> True
isEmpty _ = False
isEmpty [] = True
```

##### Variable is a "catch-all" pattern
Variables will behave like the **undescore pattern**, so the following example will also always match the first pattern since *l* can be anything.
```Haskell
headOrDef :: Int -> [Int] -> Int
headOrDef def l = def
headOrDef _ (x : _) = x
```

##### Patterns on variable names are NOT supported
Sadly, pattern matching in Haskell doesn't work on variable name: if we write the same name Haskell doesn't get we mean equality. The following example won't work as we wanted.
```Haskell
same :: Int -> Int -> Bool
same x x = True
same _ _ = False
```

##### Patterns don't cover all possible cases by default
We need to make sure the pattern we wrote covers all cases. For example, in the following code we don't cover the possibility of empty lists.
```Haskell
head :: [Int] -> Int
head (x : _) = x
```

##### Not an error, but pattern matching for Strings is TERRIBLE
To use pattern matching with a String, we need to think of it as a list of Char, which gives us ugly monsters like the following one.
```Haskell
isGreeting :: String -> Bool
isGreeting str = case str of
    'H' : 'i' : '!' : _ -> True
    _ -> False
```
