# Haskell Introduction
Haskell is a general-purpose purely functional programming language, statically-typed, with type interface and lazy evaluation.

## Functional programming languages
In functional programming the main building block of programs is **functions**. And it should be easy to:

- **define** functions
- **call** functions
- **compose/combine** functions
- **pass arguments to** functions
- **inspect** functions
- **define new functions with existing ones**

### Main functional programming concepts

#### Higher-Order Functions (HOF)
Functions can take other funcions as arguments.

#### Algebraic Data Types (ADT)
ADT is a way to create very flexible data types.

#### Pattern matching
The ability to describe patterns for your inputs and how your functions behave on this patterns.

#### Purely functional
Functions shouldn't have any side-effects.

Every function in Haskell is a function in the mathematical sense (i.e., "pure"). Even side-effecting IO operations are but a description of what to do, produced by pure code. There are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers.

#### Immutability
All values are immutable. Existing variables and data structures cannot be modified.

#### Totality
A function is **total** if it is defined for all inputs of its corresponding type, or in other words, if a function returns the output on any possible values of the input types.

Functions should work on **all** inputs of its type, not throwing exceptions or missing particular cases not covered.

### Additional Haskell features

#### Statically typed
Every expression in Haskell has a type which is determined at compile time. All the types composed together by function application have to match up. If they don't, the program will be rejected by the compiler. Types become not only a form of guarantee, but a language for expressing the construction of programs.

#### Laziness
Data streams are only processed when necessary.

Functions don't evaluate their arguments. This means that programs can compose together very well, with the ability to write control constructs (such as if/else) just by writing normal functions. The purity of Haskell code makes it easy to fuse chains of functions together, allowing for performance benefits.

#### Polymorphisms
Haskell allows to write types of functions that work not only for single types but for different types with polymorphic functions.

#### Type inference
You don't have to explicitly write out every type in a Haskell program. Types will be inferred by unifying every type bidirectionally. However, you can write out types if you choose, or ask the compiler to write them for you for handy documentation.

#### Layout-sensitivity
You don't need to specify blocks of code with braces, you specify them with the same indentation (like in Python).

Even if not needed, you can still use them after several keywords: **where**, **let**, **do** and **of**.

#### Meta language (ML) syntax
General-purpose, high-level functional programming language, with polymorphic type systems, type inference, type safety, pattern matching, garbage collection, imperative programming, call-by-value, first-class functions, ADT and currying.

Languages of the ML family are for example OCaml, SML or F\#.

Haskell is influenced by ML but purely functional and with no side-effects.

#### Automatic currying
Haskell allows to apply functions partially and get other functions.

#### Garbage collector
Haskell has its own runtime system with a garbage collector taking care of memory management.

#### Green Threads
Haskell allows creating threads and also has **green** or **lightweight** threads: small lines of work that are scheduled down onto actual execution contexts, set by default to be one per core, and incredibly lightweight. Haskell provides a rich ecosystem of tools to do work concurrently and to communicate safely between threads.

#### Concurrent
Haskell lends itself well to concurrent programming due to its explicit handling of effects. Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and light-weight concurrency library containing a number of useful concurrency primitives and abstractions.

#### Software Transactional Memory (STM)
Concurrency control mechanism for controlling access to shared memory and concurrent computing.
