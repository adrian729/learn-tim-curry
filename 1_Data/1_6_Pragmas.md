# Haskell pragmas

## {-# MINIMAL #-}
As seen when checking **classes** and **polymorphism**, it allows to declare which is the minimal implementation of a class.

## {-# LANGUAGE #-}
Enables GHC additional features.

### InstanceSigs
```Haskell
{-# LANGUAGE InstanceSigs #-}
```
```Haskell
{-# LANGUAGE InstanceSigs #-}

module Display where

class Display a where
  display :: a -> String

instance Display Char where
    display :: Char -> String -- <- pragma needed for this!
    display c = [c]
```
Allows to write the type signature for the instance method (for some reason, by default it is not allowed when implementing instances).

