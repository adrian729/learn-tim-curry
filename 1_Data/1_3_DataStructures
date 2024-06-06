# Data sctructures

## Tuples
Tuples are similars to lists, but can have elements of different types!

### syntax
```Haskell
(elem1, elem2, ..., elemN)
```

### Methods
This only works with tuples of EXACTLY two elements.
```Haskell
ghci> fst ('x', True)
'x'
ghci> snd ('x', True)
True
```

### Working with tuples

#### returning
```Haskell
splitAtPos3 :: [Int] -> ([Int], [Int])
splitAtPos3 l = (take 3 l, drop 3 l)
```

#### matching
```Haskell
showTriple :: (Bool, Int, String) -> String
showTriple (b, n, string) =
  if b
    then "The number is: " ++ show n
    else "The string is: " ++ string
```

```Haskell
ghci> showTriple (False, 42, "hello")
"The string is: hello"
ghci> showTriple (True, 42, "hello")
"The number is: 42"
```

## Algebraic Data Type (ADT)
A type formed by combining other types using either *product* or *sum* types.

By using the **data** we can create our own types. This way we can acomplish ADTs and more complex data structures.

### Product type
- Combining types using both
- Zero or more types combined

#### data: Product
```Haskell
data User = MkUser String Int Bool
```
- **User**: *data type* name
- **MkUser**: *constructor* name
- **String Int Bool** *field* types

Our data type User will have 3 fields, one of type String, one Int and one Bool.

Getters and setters for the new data type.
```Haskell
data User = MkUser String Int Bool
  deriving (Show) -- to display our type in GHCi

-- Getters
getUserName :: User -> String
getUserName (MkUser name _ _) = name

getUserAge :: User -> Int
getUserAge (MkUser _ age _) = age

-- Setters - values in Haskell are immutable! So return a new User
setUserName :: String -> User -> User
setUserName name (MkUser _ age isTired) = MkUser name age isTired
```

##### data: Records
If there is to many fields the syntax we have seen until now will be really inconvenient, Haskell has **Records** for that.
```Haskell
data User = MkUser
  { userName :: String,
    userAge :: Int,
    userIsTired :: Bool
  }
```

This syntax also autogenerates top-level functions, so no need to write the basic getters.
```Haskell
userName :: User -> String
userAge :: User -> Int
userIsTired :: User -> Bool
```

It also lets creating new records from other records, by writing the other value name and after it the changed values in curly braces.
```Haskell
ghci> john = MkUser {userName = "John", userAge = 30, userIsTired = True}
ghci> userName john
"John"
ghci> kevin = john {userName = "Kevin", userAge = 20}
ghci> userName kevin
"Kevin"
```

### Sum type
Choice of types, either one or another

#### Defining a sum type
```Haskell
data Result
  = Error String
  | Ok Int
```

```Haskell
divide :: Int -> Int -> Result
divide _ 0 = Error "Division by zero!"
divide x y = Ok (div x y)
```

#### Enumerations
Enumerations are an example of sum type
```Haskell
data Color
  = Red
  | Blue
  | Green
```

```Haskell
showColor :: Color -> String
showColor color = case color of
  Red -> "red"
  Blue -> "blue"
  Green -> "green"
```

## Recursive data types
```Haskell
data IntList
  = Empty
  | Cons Int IntList
```
- **Cons**: *constructor* name. The name *Cons* is arbitrary and could be anything, like *List* or *Array*, but *Cons* is a common name historically used in Lisp.
- **:-:**: is a special name for a constructor that can be used *infix*.
```Haskell
data IntList
  = Empty
  | Int :-: IntList
```

```Haskell
length :: IntList -> Int
length Empty = 0
length (Cons _ xs) = 1 + length xs
```

```Haskell
nzeroes :: Int -> IntList
nzeroes 0 = Empty
nzeroes n = Cons 0 (nzeroes (n - 1))
```

```Haskell
ghci> length (nzeroes 25)
25
```

## Renaming types
Haskell allows to give types another name with the **type** keyword.
```Haskell
type MyTriples = [(Int, Int, Int)]
type IntPredicate = Int -> Bool
```

### Problems with renaming
Even if it is renamed, the type is the same!

For example:
```Haskell
type Volts = Int
type Amps = Int
type Ohms = Int
```

```Haskell
intensity :: Volts -> Ohms -> Amps
intensity v r = v `div` r
```

The problem is that, even if renamed, Volts, Amps and Ohms are all Ints. So we could call intensity with wrong Int parameters - for example, the Volts and Ohms swapped - without the compiler noticing.

## newtype
Lightweight wrapper for existing type. It can only have:
- Exactly one constructor
- Exactly one field

```Haskell
newtype Volts = MkVolts Int
newtype Amps =  MkAmps Int
newtype Ohms =  MkOhms Int
```
The difference between **newtype** and **type** is that **newtype** creates a new type, while **type** creates a synonym for an existing type. With **newtype** the compiler will be able to catch errors when using wrong types, even if they derive from the same base type.

On the other side, to use **newtype** we need to add some boilerplate.
```Haskell
intensity :: Volts -> Ohms -> Amps
intensity (MkVolts v) (MkOhms r) = MkAmps (v `div` r)
```

### Pros
- No runtime cost
- Really a new type

### Cons
- Wrapping and unwrapping - boilerplate
