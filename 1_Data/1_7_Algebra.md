# Algebra
Math and algebra related typeclasses.

This classes must follow some **laws/rules** to be valid.

It is important to note that those laws are **NOT** enforced by the compiler, but they are important to ensure that the instances behave as expected.

## Semigroup - appendable
```Haskell
class Semigroup a where
    (<>) :: a -> a -> a
```
or the equivalent class

```Haskell
class Appendable a where
    append :: a -> a -> a
```

### Laws/rules
- **Associativity**: $x <> (y <> z) \equiv (x <> y) <> z$

Semigroups should follow associativity: the order of operations should not matter.

### Standard instances
```Haskell
instance Semigroup [a] where
    (<>) = (++)
```

But what for Bool?
```Haskell
instance Semigroup Bool where
  (<>) = ??? -- && or ||, which one to choose?
```
With `newtype` we can implement different behaviours for the same type.

```Haskell
newtype Any = Any { getAny :: Bool }
newtype All = All { getAll :: Bool }

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Semigroup All where
    All a <> All b = All (a && b)
```
Careful, then our `Bools` will be of type `Any` or `All`, so we can't mix them.

## Monoid - semigroup with identity
```Haskell
class Semigroup a => Monoid a where
    mempty :: a
```

### Laws/rules
- **Right identity**: $x <> mempty \equiv x$
- **Left identity**: $mempty <> x \equiv x$

### Standard instances
```Haskell
instance Monoid [a] where
    mempty = []
```

```Haskell
instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Monoid (Product a) where
    mempty = Product 1
```

```Haskell
instance Monoid Any where
    mempty = Any False

instance Monoid All where
    mempty = All True
```

## Kind
Type of a type.

```Haskell
ghci> :k Int
Int :: *
ghci> :k String
String :: *
ghci> :k Maybe
Maybe :: * -> *
ghci> :k []
[] :: * -> *
```
Types like Maybe, Either, list, which have a `* -> *` kind, are called **type constructors**. They are not complete types by themselves, they need to be **applied** to other types.

The following won't work, because the type signature expects two types of kind `*`, but `Maybe` is of kind `* -> *`:
```Haskell
maybeToList :: Maybe -> [a]
```

### Polymorphism over type constructors
Example: a typeclass for creating singleton "containers" from values.

```Haskell
class Singleton f where
    singleton :: a -> f a
```

```Haskell
ghci> :t singleton
singleton :: Singleton f => a -> f a
```

- `f` is a type constructor, so it has kind `* -> *`
- `a` is a type, so it has kind `*`
- Value types inside the type constructor must be of the same type `a`

```Haskell
instance Singleton Maybe where
    singleton :: a -> Maybe a
    singleton x = Just x
```

```Haskell
instance Singleton [] where
    singleton :: a -> [a]
    singleton x = [x]
```

## Functor
Mapping values inside context `f`

```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
**fmap** is a generalization of the `map` function.

### Laws/rules
- **Identity**: fmap id $\equiv$ id
- **Composition**: $\text{ fmap } (f . g) \equiv \text{ fmap } f . \text{ fmap } g$

Identity function:
```Haskell
id :: a -> a
id x = x
```

Correct:
```Haskell
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
```

Incorrect:
```Haskell
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f m = Nothing
```
It is incorrect because it breaks the laws!

## Folds

## Fold right
`f x_1 (f x_2 (f x_3 ... (f x_n acc)))`

It will apply the function `f` to the elements of the list from right to left, accumulating the result. The first call to `f` will use the initial value `acc`.

```Haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)
```

- `(a -> b -> b)` "step" function
- `-> b ->` "accumulator" or initial value
- `[a]` list to fold
- ` -> b` final result

```Haskell
ghci> foldr (+) 0 [1 .. 5]
15
ghci> foldr (*) 3 [2, 4]
24
```

- Folds are very generic and can implement a lot of things (regarding lists at least). That doesn't mean it is always the best choice! Explicit recursion can be more readable in some cases

- Use `foldr` when the function is **lazy on the second argument**

```Hasell
ghci> foldr (&&) True (repeat False)
False
ghci> foldr (\x _ -> Just x) Nothing [1 .. ]
Just 1
```

## Fold left
`f (... (f (f acc x_1) x_2) x_3 ...) x_n`

It will apply the function `f` to the elements of the list from left to right, accumulating the result. The first call to `f` will use the initial value `acc`.

```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs
```

### Memory leak!
Unfortunately, `foldl` **always** leaks memory due to its nature, so you should **NEVER** use it, use `foldl'` instead.

**foldl'** is a strict version of **foldl** that doesn't leak memory.

## Foldable
Folding any structure `t` that contains elements
```Haskell
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

    foldMap :: Monoid m => (a -> m) -> t a -> m

    -- ...and more methods...
```

With that some function types make more sense now:
```
ghci> :t sum
sum :: (Foldable t, Num a) => t a -> a
ghci> :t concat
concat :: Foldable t => t [a] -> [a]
```
