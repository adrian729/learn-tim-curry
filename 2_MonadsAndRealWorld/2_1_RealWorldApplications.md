# Abstraction for real world applications

## Purity
**Pure functions** are functions without side-effects that depend only on their explicit input arguments.

### Benefits of purity
- Determinism: same input always produces the same output
- Easier reasoning: no need to consider side-effects
- Simple testing: no need to setup environment, just pass arguments
- Composability: composing pure functions gives pure functions
- Optimization: compiler can optimize pure functions
- Parallelism: pure functions can be run in parallel easily

### Problems with purity
Purity sounds really nice, but for real world applications we need some side-effects: to interact with input/output devices, databases, network, etc.

### Purity + Laziness
Let's give an example, how would a read line look like in a pure and lazy language?
```Haskell
getLine :: String
```
Nice! How about reading two lines?
```Haskell
getTwoLines :: [String]
getTwoLines = [getLine, getLine]
```
Amazing, but.. what if?
```Haskell
ghci> getTwoLines !! 1
```
We are trying to get the second line by forcing it, but Haskell is lazy! It will only execute getLine once, so it won't actually read the line we want!

So, we can't have side-effects in Haskell without changing some things, otherwise we are introducing unexpected behaviours for us!

### Input/Output (IO)
**IO** was added to be able to introduce some side-effects in Haskell and be able to deal with the real world.
```Haskell
getLine :: IO String
```
`getLine` is a function that returns a value of type **String** and also will perform *some* side-effects.

Also, **IO** has a **Monad** instance:
```Haskell
instance Monad IO where
    return :: a -> IO a
    (>>=) :: IO a -> (a -> IO b) -> IO b
```

Now, to get two lines:
```Haskell
getTwoLines :: IO [String]
getTwoLines =
    getLine >>= \line1 -> getLine >>= \line2 -> pure [line1, line2]
```

#### How to run IO

#### putStrLn/print
`putStrLn` takes a value of type **String**, prints it to the terminal and returns a **unit type** `()`.
```Haskell
putStrLn :: String -> IO ()
```
```Haskell
print :: Show a => a -> IO ()
```

The `()` or **unit type** is a type with only one value, also called `()`.

It is used because in Haskell every function must return a value, so if a function doesn't have a meaningful value to return, it returns `()`.

#### getLine
Get a line and return it as a **String**.
```Haskell
getLine :: IO String
```

## How to run real haskell programs?
This was fun, but how do we actually run Haskell programs?

Haskell programs start with the `main` function in the module **Main**.

```Haskell
-- HelloWorld.hs
module Main where

main :: IO ()
main = putStrLn "Hello, World!"
```
Example of a simple Hello World program in Haskell.

### Compilation
To compile a Haskell program, we use the `ghc` compiler.
```bash
$ ghc HelloWorld.hs -o hello-world
[1 of 2] Compiling Main             ( HelloWorld.hs, HelloWorld.o )
[2 of 2] Linking hello-world.exe

$ ./hello-world
"Hello World"
```

## Then operator (>>)
The `>>` operator is a shorthand for the pattern: **binding to a lambda and ignoring its argument.**
```Haskell
(>>) :: Monad m => m a -> m b -> m b
action1 >> action2 = action1 >>= \_ -> action2
```

### Example
Imagine we want to print two lines. We can use the **bind operator** and **putStrLn**, ignoring the return value of the first action, since it returns `()`.
```Haskell
module Main where

main :: IO ()
main = putStrLn "Hello" >>= \_ -> putStrLn "World"
```

This can be done with the `>>` operator.
```Haskell
module Main where

main :: IO ()
main = putStrLn "Hello" >> putStrLn "World"
```

For **IO** and actions that have side-effects but no return it makes sense to use `>>` to simplify the code a bit.

## do-notation
**do-notation** is syntax sugar for `>>=`, `>>` and **let-in**.

Use what you feel more comfortable with.

### do-notation with `>>=`
```Haskell
example = f1 >>= \res -> f2 res
```
With **do-notation**:
```Haskell
example = do
    res <- f1
    f2 res
```
Without **do-notation**, but **eta-reduction**:
```Haskell
example = f1 >>= f2
```

### do-notation with `>>`
```Haskell
example = f1 >> f2
```
With **do-notation**:
```Haskell
example = do
    f1
    f2
```

### do-notation with **let-in**
```Haskell
example = let x = f y in fun x
```
With **do-notation**:
```Haskell
example = do
    let x = f y
    fun x
```

### Other examples
```Haskell
main :: IO ()
main =
    getLine >>= \line ->
    let rev = reverse line in
    putStrLn rev
```
With **do-notation**:
```Haskell
main :: IO ()
main = do
    line <- getLine
    let rev = reverse line
    putStrLn rev
```

#### Common mistakes
1. Trying to create a variable with a funcion
```Haskell
let line = getLine
```
This will just assign the function `getLine` to `line`, not the result of the function.
2. Trying to **bind** pure functions
```Haskell
rev <- reverse line
```
This will not work, because `reverse` is a pure function, not an **IO** action.

# Cabal
- **.cabal** is a format for describing structure of Haskell packages.
- Files with **.cabal** extension describe the package
- **cabal-install** is a Haskell build tool that works with Cabal packages

## Structure
project-name/
    ├── app/
    │   └── Main.hs
    ├── src/
    │   ├── Module.hs
    │   └── AnotherModule.hs
    ├── test/
    │   └── Spec.hs
    ├── project-name.cabal
    └── Setup.hs

### Main best practices
It is encouraged to keep the **Main** module as small as possible, implementing the actual logic in another module.
```Haskell
module Main (main) where

import qualified MyModule

main :: IO ()
main = MyModule.main
```

### build-depends
**build-depends** is a field in the **.cabal** file that specifies the dependencies of the package.

#### Common Haskell packages
- **base**: standard Haskell library
- **containers**: Map and Set data structures
- **text**: efficient UTF-16 and UTF-8 strings
- **bytestring**: efficient byte arrays
- **time**: time, clocks and calendar data types and functions
- **aeson**: JSON parsing and encoding


# Writting Haskell programs
Haskell allows **controlling** side-effects. Separate pure logic from effectful computations and require **IO** explicitly and only when needed.

## Paradigm: Funcional core, Imperative shell
Push side-effects to the boundaries of your program, keeping your functions (pure) as your core main logic.

### Example

In **imperative style** we would have:
```Haskell
readFile :: FilePath -> IO Text
countWords :: Text -> IO (Map Text Int)
printWord :: Map Text Int -> IO ()
```
Following **functional core, imperative shell**:
```Haskell
readFile :: FilePath -> IO Text

countWords :: Text -> Map Text Int
findFrequent :: Map Text Int -> Maybe (Text, Int)
displayResult :: Maybe (Text, Int) -> Text

putStrLn :: Text -> IO ()
```
** Maybe from the Map to handle the case when the file was empty.

Basically, separation of concerns: **pure functions** for logic, **IO** for side-effects.
