## Control flow

### if-then-else
```
-- Return first elem of the list or the given default val
headOrDefault :: Int -> [Int] -> Int
headOrDefault def list = if null list then def else head list
-- Also can be done multiline
headOrDefaultMultiLine :: Int -> [Int] -> Int
headOrDefaultMultiLine def list =
    if null list
    then def
    else head list
```

### guards
Conditional matching, guards represented by **|**

Syntax is: guard condition = result

```
sign :: Int -> String
sign n
    | n == 0    = "Zero"
    | n < 0     = "Negative"
    | otherwise = "Positive"
```

### let-in
```
let var = expr in result
```
or multiline
```
let var1 = expr1
    var2 = expr2
in result
```
Example:
```
sameThreeAround :: [Int] -> Bool
sameThreeAround list =
    let firstThree = take 3 list
        lastThree  = reverse (take 3 (reverse list))
    in firstThree == lastThree
```

### where
```
appendLastTwos :: [Int] -> [Int] -> [Int]
appendLastTwos list1 list2 = lastTwo list1 ++ lastTwo list2
    where
        lastTwo :: [Int] -> [Int]
        lastTwo l = reverse (take 2 (reverse l))
```