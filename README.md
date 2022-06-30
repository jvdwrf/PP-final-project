# PP Final Project: FML - FileMasterLang

This folder contains the entire compiler for FML

## Prerequisites

Make sure you have installed:

- Stack (<https://docs.haskellstack.org/en/stable/README/>). Preferably 2.7 or higher.

## Compiling

In a terminal, run:

```
stack build
```

## Running

In a terminal, run:

```
stack run -- <path-to-file>
```

## Tests

For unit tests, run:

```
stack test
```

For manual tests, run:

```
stack run -- ExamplePrograms/<filename>.fml
```

This will print out the expected output, which can be compared directly to the actual output.
