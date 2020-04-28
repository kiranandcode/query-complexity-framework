# Query Complexity Framework
The following is a simple framework for the empirical exploration of
query complexity questions.

The project consists of an abstract framework for programatically
analyzing query complexity functions, and provides a number of example
instantiations of the framework on a few generalizations of GPW to 3D.


## Project structure
```
.
|-- dune
|-- dune-project
|-- gpw.ml
|-- majority_gpw.ml
|-- majority_alg.ml
|-- generalized_gpw.ml
|-- utils.ml
|-- main.ml
`-- readme.md
```

The framework is logically decomposed into small modular reusable
components by file:
- `gpw.ml` - Defines the abstract interface of a query complexity
  problem generator - any such component should provide functions to
  generate true, false and random values.
- `majority_gpw.ml` - Implementation of a problem generator for an
  extension of the Göös-Pitassi-Watson function to 3D by taking
  the majority of multiple instances of 2D GPW on seperate planes.
- `majority_alg.ml` - Implementation of a solver algorithm for
  majority GPW, implementing Muks et al.'s 1-sided GPW solution
  strategy internally for each plane.
- `generalized_gpw.ml` - Implementation of a problem generator for an
  extension of the Göös-Pitassi-Watson function to 3D by replacing columns
  with sub-cubes of the input instead.
- `utils.ml` - Utility functions used throughout the project
- `main.ml` - Example instantiations of the framework to run some experiments

## Requirements
In order to run this project, you must have  OCaml installed  (version `>= 4.09.0`)
with opam, and the following packages installed:
- `core`
- `gnuplot`

## Running
To run the sample experiments, simply run the following from the project root:
```
dune exec ./main.exe
```
