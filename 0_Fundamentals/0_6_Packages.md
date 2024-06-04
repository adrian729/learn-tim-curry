## Packages
- **module**: a collection of fs and custom data types
- **package**: collection of modules + metadata
- **Hackage**: a central repo of Haskell packages
- **base**: standard Haskell library
- **Prelude**: the module from **base** imported by default

Example import a package:
```
ghci> import Data.List
ghci> sort [3, 1, 2]
[1,2,3]
ghci> nub [True, False, True, True]
[True,False]
```
