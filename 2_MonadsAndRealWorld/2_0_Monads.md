
# Monads

## Why Monads?
Let's see different examples with a particular problem.

### Maybe
Imagine something like this, where we have several inputs and we need to check for combinations of them to know the result.
```Haskell
maybePlus :: Maybe Int -> Maybe Int -> Maybe Int
maybePlus ma mb = case ma of
    Nothing -> Nothing
    Just a -> case mb of
        Nothing -> Nothing
        Just b -> Just (a + b)
```

For only two inputs it is a bit messy but feasible. But what if we have more inputs? We would have to nest more and more `case` expressions.

One option can be using a helper function:
```Haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen ma f = case ma of
    Nothing -> Nothing
    Just x -> f x
```
```Haskell
maybePlus :: Maybe Int -> Maybe Int -> Maybe Int
maybePlus ma mb = andThen ma (\a -> andThen mb (\b -> Just (a + b)))
```

### Either
```Haskell
eitherPlus :: Either String Int -> Either String Int -> Either String Int
eitherPlus ea eb = case ea of
    Left err -> Left err
    Right a -> case eb of
        Left err -> Left err
        Right b -> Right (a + b)
```

Again, same problem, same solution:
```Haskell
andThen :: Either e a -> (a -> Either e b) -> Either e b
andThen ea f = case ea of
    Left err -> Left err
    Right x -> f x
```
```Haskell
eitherPlus :: Either String Int -> Either String Int -> Either String Int
eitherPlus ea eb = andThen ea (\a -> andThen eb (\b -> Right (a + b)))
```

### List
```Haskell
listPlus :: [Int] -> [Int] -> [Int]
listPlus la lb = case la of
    [] -> []
    a : as -> case lb of
        [] -> []
        bs -> map (+ a) bs ++ listPlus as bs
```
Again:
```Haskell
andThen :: [a] -> (a -> [b]) -> [b]
andThen l f = case l of
    [] -> []
    x : xs -> f x ++ andThen xs f
```
```Haskell
listPlus :: [Int] -> [Int] -> [Int]
listPlus la lb = andThen la (\a -> andThen lb (\b -> [a + b]))
```

### Similar pattern
All these examples have a similar pattern/type signatures, and the solution also:
```Haskell
andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen :: Either e a -> (a -> Either e b) -> Either e b
andThen :: [a] -> (a -> [b]) -> [b]
```

And the final implementation is also almost the same for all of them:
```Haskell
maybePlus ma mb = andThen ma (\a -> andThen mb (\b -> Just (a + b)))
eitherPlus ea eb = andThen ea (\a -> andThen eb (\b -> Right (a + b)))
listPlus la lb = andThen la (\a -> andThen lb (\b -> [a + b]))
```

Recognizint this pattern, we can abstract it!

## Monad
Monad is a pattern!

**Monad** is a generalization of the pattern we have seen with the *andThen* function.

How do we generalize?
- Polymorphic **andThen** function
- Polymorphic **constructor**

### Monad typeclass
```Haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

- Monad is just a **typeclass**
- It has two methods:
    - **return** is a polymorphic constructor (not the same as `return` in imperative languages)
    - **(>>=)** is the **bind** operator
- It is a typeclass for **type constructors**, like `Maybe`, `Either`, `[]`

### Monad instances
```Haskell
instance Monad Maybe where
    return :: a -> Maybe a
    return x = Just x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f = f x
```
```Haskell
instance Monad (Either e) where
    return :: a -> Either e a
    return x = Right x

    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    Left err >>= _ = Left err
    Right x >>= f = f x
```
```Haskell
instance Monad [] where
    return :: a -> [a]
    return x = [x]

    (>>=) :: [a] -> (a -> [b]) -> [b]
    l >>= f = concatMap f l
```

### Monad laws
- **Left identity**: `return x >>= f` $\equiv$ `f x`
- **Right identity**: `m >>= return` $\equiv$ `m`
- **Associativity**: `(m >>= f) >>= g` $\equiv$ `m >>= (`$\lambda$`x -> f x >>= g)`

### Refactoring the examples
```Haskell
maybePlus :: Maybe Int -> Maybe Int -> Maybe Int
maybePlus ma mb = andThen ma (\a -> andThen mb (\b -> Just (a + b)))
maybePlus ma mb = ma `andThen` (\a -> mb `andThen` (\b -> Just (a + b))) -- infix notation
maybePlus ma mb = ma `andThen` \a -> mb `andThen` \b -> Just (a + b) -- removing parentheses
maybePlus ma mb = ma >>= \a -> mb >>= \b -> return (a + b) -- replace: andThen with bind operator, Just with return
-- this is not depending on Maybe anymore, so we can generalize it
monadPlus :: Monad m => m Int -> m Int -> m Int
monadPlus ma mb = ma >>= \a -> mb >>= \b -> return (a + b)
```

### Monad FAMily
Monad is a part of FAM (Functor, Applicative, Monad) family.
```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b
```

To implement a monad instance, we need to implement an applicative instance, and to implement an applicative instance, we need to implement a functor instance.

**pure** is the same as **return**, but nowadays everybody is using **pure**.


