
# Common properties
A problem with parametric polymorphsim is that it can't be always used.

For example, having:
```Haskell
maxInt :: Int -> Int -> Int
maxInt x y = if x > y then x else y
```

```Haskell
maxChar :: Char -> Char -> Char
maxChar x y = if x > y then x else y
```

It would be nice to be able to just write:
```Haskell
max :: a -> a -> a
max x y = if x > y then x else y
```

What is the problem with this? It implies the function will work with **ANY** type, but we don't know if the type has an `>` operator defined or how to compare two values of that type!

That is not the case in the following example:
```Haskell
take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs
```

This function will work for any list, whatever the type of `a` is.

What is the difference between the two examples? It is two kinds of polymorphism:
- **Parametric polymorphism**: same behaviour for any type.
- **Ad-hoc polymorphism**: different behaviour for different types.

### Parametric polymorphism examples
- Get first element of a pair
- Reversing lists
- List length
- Take X elements from a list
- Function composition

### Ad-hoc polymorphism examples
- Number addition
- Equality
- Comparison
- Convertion to string
- Parsing from a string

## Parametric polymorphism
Parametric polymorphism allows us to create functions that are type-agnostic by using `type variables`. This means that we can create a function that works with any type as long as it is consistent: all the cases of the same `type variable` must be of the same type.

The convention is that:
- Specific types start with an uppercase letter: `Int`, `String`, `Bool`, etc.
- Type variables start with a lowercase letter: `a`, `b`, `c`, etc.

```Haskell
dup :: a -> (a, a)
dup x = (x, x)
```

```Haskell
ghci> dup 'x'
('x','x')
ghci> dup 3
(3,3)
ghci> dup True
(True,True)
ghci> dup 2.4
(2.4,2.4)
```

## Ad-hoc polymorphism
Haskell supports ad-hoc polymorphism through type classes.

### Type classes
```Haskell
class Display a where
  display :: a -> String
```
- `class` keyword is used to define a type class
- `Display` Typeclass name
- `a` Type variable
- `display :: a -> String` function name and type signature

#### Type class instances
To implement classes, we need to define instances for the types we want to support.

```Haskell
instance Display Bool where
  display False = "false"
  display True = "true"
```

```Haskell
instance Display Char where
  display c = [c]
```
Here we use the definition of String: a String is a list of characters, so we only need to put the character in a list.

##### Using instances
```Haskell
greet :: Display a => a -> String
greet x = "Hello, " ++ display x ++ "!"
```

```Haskell
displayBoth :: (Display a, Display b) => a -> b -> String
displayBoth x y = display x ++ " and " ++ display y
```
### Separation of concerns
- **data**: what is stored inside?
- **class**: what can be done with it?
- **instance**: how does it behave for a specific data?

## Default methods
```Haskell
class Display a where
  {-# MINIMAL display #-}

  display :: a -> String

  displayList :: [a] -> String
  displayList l =
    "[" ++ intercalate ", " (map display l) ++ "]"
```
- `{-# MINIMAL display #-}`: `MINIMAL` pragma specifies the minimum methods that need to be implemented for the class to be valid. It is only for documentation/helping knowing what is needed, the compiler doens't check it.
- `displayList`: default implementation for the display of a list of elements. It will work for any type which implements `Display`.

### Big typeclasses
- More performant
- Allows to implement it differently for different types

### Small typeclasses
- Smaller possibility of errors
- Easier to write instances

# Standard Typeclasses
- `Eq` - check equality
- `Ord` - compare
- `Show` - convert to string
- `Read` - parse from string
- `Bounded` - has min and max values
- `Enum` - is an enumeration
- `Num` - is a number (implements `+`, `-`, `*`, etc)


## Polymorphic types
Types can also be polymorphic.

For example:
```Haskell
data Box a = MkBox
  { value :: a
  }
```

```Haskell
ghci> MkBox 100
MkBox {value = 100}
ghci> MkBox 'a'
MkBox {value = 'a'}
```

This can be mixed with parametric polymorphism:
```Haskell
type IntBox = Box Int
```

### Common polymorphic types

#### Maybe
```Haskell
data Maybe a
    = Nothing
    | Just a
```

#### Either
```Haskell
data Either a b
    = Left a
    | Right b
```

### Common functions

#### fromMaybeInt
```Haskell
fromMaybeInt :: Maybe Int -> Int
fromMaybeInt Nothing = 0
fromMaybeInt (Just x) = x
```

#### find
```Haskell
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs
```

```Haskell
ghci> find (> 4) [3, 1, 2]
Nothing
ghci> find (< 4) [3, 1, 2]
Just 3
```

