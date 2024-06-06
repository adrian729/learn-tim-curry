{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use splitAt" #-}
module Example where

increase :: Integer -> Integer
increase x = x + 1

satisfies :: (Int -> Bool) -> Int -> String
satisfies check n
  | check n = "The number " ++ show n ++ " passes check"
  | otherwise = "The number " ++ show n ++ " doesn't pass"

applyTwice :: (Integer -> Integer) -> Integer -> Integer
applyTwice f x = f (f x)

atLeastTwo :: [Int] -> Bool
atLeastTwo (x0 : x1 : xs) = True
atLeastTwo _ = False

headOrDef :: Int -> [Int] -> Int
headOrDef def [] = def
headOrDef _ (x : _) = x

dropHead :: [Int] -> [Int]
dropHead [] = []
dropHead (_ : xs) = xs

secondIsZero :: [Int] -> Bool
secondIsZero (_ : 0 : _) = True
secondIsZero _ = False

-- very slow implementation!
sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

-- count how many times n appears in list
count :: Int -> [Int] -> Int
count n list = go 0 list
  where
    go :: Int -> [Int] -> Int
    go acc (x : xs)
      | x == n = go (acc + 1) xs
      | otherwise = go acc xs

splitAtPos3 :: [Int] -> ([Int], [Int])
splitAtPos3 l = (take 3 l, drop 3 l)

showTriple :: (Bool, Int, String) -> String
showTriple (b, n, string) =
  if b
    then "The number is: " ++ show n
    else "The string is: " ++ string

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

data Person = MkPerson
  { personName :: String,
    personAge :: Int,
    personIsTired :: Bool
  }

data Color
  = Red
  | Blue
  | Green

showColor :: Color -> String
showColor color = case color of
  Red -> "red"
  Blue -> "blue"
  Green -> "green"

data Result
  = Error String
  | Ok Int

divide :: Int -> Int -> Result
divide _ 0 = Error "Division by zero!"
divide x y = Ok (div x y)

data IntList
  = Empty
  | Cons Int IntList
  deriving (Show)

intListLength :: IntList -> Int
intListLength Empty = 0
intListLength (Cons _ xs) = 1 + intListLength xs

nzeroes :: Int -> IntList
nzeroes 0 = Empty
nzeroes n = Cons 0 (nzeroes (n - 1))

newtype Volts = MkVolts Int

newtype Amps = MkAmps Int

newtype Ohms = MkOhms Int

intensity :: Volts -> Ohms -> Amps
intensity (MkVolts v) (MkOhms r) = MkAmps (v `div` r)

dup :: a -> (a, a)
dup x = (x, x)

data Box a = MkBox
  { value :: a
  }
  deriving (Show)

-- Need to check how to actually use this...
type IntBox = Box Int

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

takeEven5 :: [[a]] -> [[a]]
takeEven5 list = take 5 (filter (even . length) list)