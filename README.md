# Haskell solutions for Advent of code 2022
This library contains a module with solution for each day of 2022 Advent of code,

## Usage
* To interact with with this library, run `cabal repl` in the project directory.
* In the repl, use `solve` function, that accepts two arguments: 
	* A function (`Show b => (String -> b)`) that solves a problem.
	* A path to an input file as a string.

* E.g. to solve day's 1 part 1: `solve Day1.part1 "path/to/your/input.txt"`