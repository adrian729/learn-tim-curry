module Main where

helloWorld :: IO ()
helloWorld = print "Hello World"

main :: IO ()
main = do
  helloWorld
