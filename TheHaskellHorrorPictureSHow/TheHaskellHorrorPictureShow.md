# The Haskell Horror Picture Show - Haskell a cult classic
Haskell, the language that everyone loves to hate. Monads, Monoids, Functors, Functoids, Applicatives, Applications... To be honest I invented some of the words in there and am not even sure if maybe are real.

Also, just wtf, right? Do we need all this fancy words to start programming in Haskell? Short answer: no. Long answer: also no.

I mean I get it, fancy words are fun, but we are pragmatic people, we want to get things done. So let's get things done.

## Haskell, the wrong way - Why?
Why would you want to learn Haskell... the wrong way?

Lets start with why not:

Did you try to learn Haskell before? No? I reccommend you go to the [Haskell documentation](https://www.haskell.org/documentation/) and learn Haskell the right way! There are many resources available to learn Haskell, and to be honest, it is better to learn any language right from the start. Go there, try it out, and if you find it frustrating, cryptic or hard, come back here, you will always be welcome.

Why? Because it is better to learn something taking some extra steps in the process than to not learn it at all.

This chapters aim to help programmers and developers already familiar with other languages to learn Haskell.

To do so we will just forget all the idiomatic, special words and characters and other weird stuff that Haskellers like to use and focus on doing some Haskell.

### Again, why this tutorial?
Haskell biggest problem, at least for me, is that it is not friendly at all for beginners, even if They do have experience with other languages. Don't get me wrong, Haskell is great, and I don't think the community itself is not friendly, but:

- The documentation, or at least actually finding it, is terrible. No offence to the Haskell community, but not even the [Haskell documentation](https://www.haskell.org/documentation/) has actual documentation on the language.
- Small comunity means less resources, less tutorials, less help, and less answers to the problems you may find, and when you do the answer and resources can be more than 10 years old, and you have to figure out if it is still valid or not.
- To write Haskell the mindset is a bit different from other languages, which can be confusing at first.
- On top of that, Haskellers like to use weird notation, special characters, and fancy words that are just cryptic and confusing for beginners. Not saying these words are there for no reason, but that doesn't make it easier to learn.
- Mathematical background, while a plus for people from scientific fields, it can be a barrier for people from other fields, and it in most cases it is not necessary to know to start programming in Haskell.
- Packages, packages, and more packages, thousandsof them, so figure out which one you should use and if it will have the features you were looking for can.

This tutorial aims to break some of these barriers and help you start programming in Haskell, because the community will only grow if more people start using learning and using it.

## What is Haskell?
- **Haskell is a functional programming language**: this means it uses functions to do kind of everything.
- **Haskell is a statically typed language**: it compiles. It checks types. It is strict about it.
- **Haskell is a lazy language**: it will not evaluate anything until it is needed, kinda like when you should do something and wait until the last minute to do it, but Haskell is actually good at it.
- **Haskell is a pure language**: it does not have side effects. Which, to be honest, if you come from another language is going to be a pain, so let's not focus on that right now.

## What do we need?
What tools do we need and can use to start programming in Haskell?

### The toolchain
- **GHC (Glasgow Haskell Compiler)**
- **GHCi (GHC interactive)**: interactive interpreter/REPL (Read-Eval-Print-Loop). Run it from a shell/terminal typing `ghci`.
  - Some useful GHCi commands:
    - `:q` quit GHCi
    - `:t` type of an expression
    - `:t +`** type of an expression with defaulting
    - `:l` load a module
    - `:i` `:info` information about a function or type
    - `:k` kind of a type
- **cabal**: Haskell build tool. To build and run bigger programs. More on that later.
- **HLS (Haskell Language Server)**: Language Server Protocol (LSP) support for Haskell
- **ghcup**: Haskell toolchain installer (for all of the above)

So start by installing [GHCup](https://www.haskell.org/ghcup/). It is a toolchain installer for Haskell. It will install GHC, GHCi, cabal, stack, HLS and other tools for you.

We will start by using GHCi, the interactive interpreter, and then write a small aberration of web application in Haskell that will make Haskellers faint and cry, but hey, if it works!


---

# Functions!
Functions are the core of Haskell, and we will start by learning how to write and use them.

## Functions in Haskell

### Syntax
In other languages you will write something like:
```Python
def functionName(arg1, arg2, ... argN):
    result = ...
    return result
```
or
```JavaScript
function functionName(arg1, arg2, ... argN) {
    result = ...;
    return result;
}
```

In Haskell you will write:
```Haskell
functionName arg1 arg2 ... argN = ...
```
No need for parenthesis, no need for return, no need for anything else. Just the function name, the arguments, `=` and the expression to be evaluated.

### Call a function
In Haskell you call a function by writing the function name followed by the arguments separated by spaces.

To check it out, go to the shell/terminal and type `ghci` to start the interactive interpreter.
```Haskell
ghci> mod 7 3
1
ghci> map length ["Hello", "world!"]
[5,6]

```

The `map` function, as in many other languages, takes two arguments, a function and a list, and applies the function to each element of the list.

## Types
Haskell is a statically typed language, which means that every expression has a type that is known at compile time.

If not explained it may look cryptic, but it is actually quite simple.

### Example
Let's look at an example and several analogies to help you understand Haskell types and functions.

The following would be a function in Haskells that adds two integers:
```Haskell
add :: Int -> Int -> Int
add a b = a + b
```

What does that mean? In haskell we write each argument type separated by `->` and the return type at the end, so the above means: the `add` function takes two `Int` arguments and returns an `Int`.

Let's compare it with other languages to make it easier to understand.

#### Analogy with C
I am not an expert in C, but hopefully if you are familiar with C this will help you understand Haskell types a bit better.

If you never did any C or similar and this doesn't help skip to the next section.

Think of the type signature of a function in Haskell as the function prototype in C, what you would find in the header file.
`add.h`:
```C
int add(int a, int b);
```
`add.c` or `add.cpp`:
```C
int add(int a, int b) {
    return a + b;
}
```
Taking a look at the Haskell version again:
```Haskell
add :: Int -> Int -> Int
add a b = a + b
```

The type signature is the same as the function prototype in C, and the implementation is the same as the function implementation in C.

Only big difference, in C/C++ the order in which you write the function is `return type` `function name` `arguments`, and in Haskell is `function name` `arguments` `return type`.

#### Analogy with TypeScript, just in case
If you are not familiar with C, hope this helps.

##### TypeScript style
```TypeScript
function add(a: number, b: number): number {
    return a + b;
}
```

##### Haskell style
```Haskell
add :: Int -> Int -> Int
add a b = a + b
```

Here it is kind of the same, but with a lot less boilerplate code in Haskell again! Which, at least for me, is a plus.

### Other examples
Let's check now what we did before, and try to infer the types of the functions and arguments:
```Haskell
map length ["Hello", "world!"]
```

#### Length
`length` takes a string and returns an integer with its length, so we would write something like this:
```Haskell
length :: String -> Int
```
#### Map
As said before, `map` takes a function and a list, and applies the function to each element of the list, so we would write something like this for the function signature:
```Haskell
map :: (String -> Int) -> [String] -> [String]
```

It looks a bit cryptic again because it has something new: parenthesis.

The parentesis mean that it expects another function with that type signature as the argument there. As you can see that is what we had as the signature for the `length` function!

In a real map, it would be something more generic, as you can expect it can take other things as arguments, but we won't cover it yet so don't worry.

It may look a bit confusing, but it is not so different from other languages.

##### TypeScript style
```TypeScript
function map(f: (s: string) => number, l: string[]): number[] {
    ...
}
```

##### C style
```C
int *map(int (*f)(char *), char **l) {
    result = ...;
    return result;
}
```
##### Java style
Probably a bit more verbose, with a lot of boilerplate code that we all love like this:
```Java
List<Integer> map(Function<String, Integer> f, List<String> l) {
    result = SuperLongDescriptiveClassName.superLongMethodNameDescribingAllItDoes.get();
    return result;
}
```

Compare now with the Haskell version of the same signature:
```Haskell
map :: (String -> Int) -> [String] -> [Int]
map f l = ...
```
Yes, it is still different than the other languages, but I hope not so much anymore, or at least not a lot more difficult to understand anymore!

At least for me, Haskell's way is more clear and concise, separating the function signature from its implementation and without so much boilerplate, but that is a matter of taste.

