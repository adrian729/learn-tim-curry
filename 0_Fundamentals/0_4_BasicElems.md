## Functions syntax
**functionName *arg1* *arg2* ... *argN***
fe: mod 7 3

Functions can be nested (use parenthesis where needed!)
fe: max (mod 7 3) (min (-5) 3)

### Lists

#### List: literals
**[]**

#### List: operators:
- **++** concat lists
- **:** add value to list front
```
ghci> [1, 3] ++ [2, 4, 5]
[1,3,2,4,5]
ghci> 6 : [2, 4, 5]
[6,2,4,5]
```

#### List: functions:
```
ghci> reverse [3, 2, 1]
[1,2,3]
ghci> take 2 [3, 2, 1]
[3,2]
ghci> drop 2 [3, 2, 1]
[1]
ghci> null []
True
ghci> null [3, 2, 1]
False
ghci> elem 3 [3, 2, 1]
True
ghci> elem 7 [3, 2, 1]
False
ghci> concat [[3, 2], [1], [], [5, 7]]
[3,2,1,5,7]
```

**NOT RECOMMENDED** functions:

*Because they may raise exceptions*
```
ghci> head [3, 2, 1]
3
ghci> tail [3, 2, 1]
[2,1]
ghci> last [3, 2, 1]
1
ghci> init [3, 2, 1]
[3,2]
```
*Very slow/inefficient + **!!** may raise exceptions if out of bounds*
```
ghci> length [3, 2, 1]
3
ghci> [3, 2, 1] !! 0
3
ghci> [3, 2, 1] !! 1
2
```

#### List: ranges
```
ghci> [1 .. 10]
[1,2,3,4,5,6,7,8,9,10]
```

You can specify a second element at the start, Haskell will calculate the diff between first and second element and generate a list with that pattern:
```
ghci> [1, 3 .. 20]
[1,3,5,7,9,11,13,15,17,19]
```

Also can do reverse. If you don't specify a second element the step by default is **+1** (so the list will be empty):
```
ghci> [10 .. 1]
[]
ghci> [10, 9 .. 1]
[10,9,8,7,6,5,4,3,2,1]
ghci> [10, 7 .. 1]
[10,7,4,1]
```

Haskell also has **INFINITE** lists:
```
ghci> [0 .. ]
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230...
```
That won't stop!

How, and why **INFINITE**? Haskell is **LAZY**, won't calculate the list until the expression is evaluated. For example, can do:
```
ghci> take 3 [0 .. ]
[0,1,2]
ghci> take 3 (drop 5 [0 .. ])
[5,6,7]
```
Being **LAZY** also means it won't crash for things that are not evaluated, for example:
```
ghci> [5, head [], 7]
[5,*** Exception: Prelude.head: empty list
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\List.hs:1646:3 in base:GHC.List
  errorEmptyList, called at libraries\base\GHC\List.hs:85:11 in base:GHC.List
  badHead, called at libraries\base\GHC\List.hs:81:28 in base:GHC.List
  head, called at <interactive>:55:5 in interactive:Ghci31
ghci> [5, head [], 7] !! 2
7
```
- first expression crashes because you can't get head from an empty list
- second only returns the elem at 2, so doesn't care or evaluate that part of the list.

### String
list of characters. Haskellers don't like it, not a really efficient representation.
- Good:
  - Can use all operations that work on **list**
  - Some extra functions

- Bad
  - Not a really efficient representation..

#### String: functions
*All list functions, plus:*
```
ghci> words "Hello, Haskell              world!"
["Hello,","Haskell","world!"]
ghci> unwords ["Hello,","Haskell","world!"]
"Hello, Haskell world!"
```

## Define a function
Syntax
```
functionName :: type
functionName arg1 ... argN = result
```
Example:
- file: Example.hs
- module: Example
```
module Example where

increase :: Integer -> Integer
increase x = x + 1
```

With **:l** we can load a file in **GHCi**:
```
ghci> :l Example.hs
[1 of 1] Compiling Example          ( Example.hs, interpreted )
Ok, one module loaded.
ghci> increase 5
6
```

## Comments
Comments start with **--**
```
-- This is a comment in Haskell
```
