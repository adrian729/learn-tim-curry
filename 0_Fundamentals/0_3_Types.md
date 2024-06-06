## Types
- *Expressions* have **types**
  - expr :: type
- use **:t** or **:type** command to view exp type in GHCI

### Types of numbers
To check the default number type you can use *:t +d* to get a more *friendly* result.
```Haskell
ghci> :t 21
21 :: Num a => a
ghci> :t +d 21
21 :: Integer
```
### Types of functions

```Haskell
ghci> :t not
not :: Bool -> Bool
ghci> :t div
div :: Integral a => a -> a -> a
ghci> :t +d div
div :: Integer -> Integer -> Integer
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
```

- **Bool -> Bool** means it is a f that gets a boolean and returns a boolean
- **Integral a => a -> a -> a**
  - **a -> a -> a** means it gets a value of type *a*, another value of type *a*, and then returns a value of type *a*
  - *Integral a* defines the type of *a*
- You can use +d to get more readable types for fs with numbers

Summarized:
- First defines the type of the generic types if needed
- Then types and arrows: each type is a parameter until the last one which is the result type
