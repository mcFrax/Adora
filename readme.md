Adora language
===

General idea
---

Adora is a programming language, developed as student project. Its aim is to
create static-typed language with as little type-related clutter and as big expressive
power as possible. It is inspired mostly by Python and C++, with some features taken from
some other languages.


Implementation
---

Interpreter implementation is written in Haskell, with parser generated by BNFC.

Interesting features
---

- Classes with properties
- Closures
- Anonymous functions
- Keyword arguments

Current state
---

Currently, Adora is able to be used as quite complete, but rather cumbersome language,
with no modules, only very basic IO, no possibility to create bindings with other
languages. Structs/classes inheritance is also not really working.

Future development
---

As the main goal of implementing interpreter - getting good mark - had been already achieved,
I lost some of the interest in developing language, and I'm not going to spend much time working
on it in the nearest future. I still hope to come back to it some day, and make it something
really usable - but it may be one of that side-projects, which wait forever, and never get finished :P

Compilation
---

To compile Adora, you need Haskell with monad library, Haskeline, BNFC, Happy, and Alex.
On debian-derived systems, `sudo apt-get install ghc libghc-mtl-dev libghc-haskeline-dev bnfc happy alex`
should install all needed dependencies.

After cloning repository, just `cd` into adora directory and run `make` to build the interpreter.

To run tests (examples from `good` and `bad` directories), do `make test`.
