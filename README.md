## HaskellFL

HaskellFL is a fault localization tool for Haskell code.

## Setup

We use Alex to build our lexer and Happy to build our Parser, so we need to install both. We can do this using cabal.

```
cabal install alex
cabal install happy
```

## Build

We can also build HaskellFL using cabal:

```
cabal build
```

## Run

To run HaskellFL, we need to call it with the Haskell source code, with the passing test cases and with the failing test cases:

```
cabal run HaskellFL faulty-code.hs tests-pass.txt tests-fail.txt
```	

For instance:

```
cabal run HaskellFL Exercises/raindrops/Raindrops.hs Exercises/raindrops/tests-pass.txt Exercises/raindrops/tests-fail.txt
```	

The command above will ask the method you want to use. Another way to use the tool is to specify the method beforehand.

```
cabal run HaskellFL Exercises/raindrops/Raindrops.hs Exercises/raindrops/tests-pass.txt Exercises/raindrops/tests-fail.txt tarantula
```	

```
cabal run HaskellFL Exercises/raindrops/Raindrops.hs Exercises/raindrops/tests-pass.txt Exercises/raindrops/tests-fail.txt ochiai
```	

We can also run the code with the test cases.

```
cabal run HaskellFL Exercises/raindrops/Raindrops.hs Exercises/raindrops/tests-pass.txt Exercises/raindrops/tests-fail.txt run convert
```	


