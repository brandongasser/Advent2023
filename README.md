# Advent of Code 2023

These are my solutions for [Advent of Code 2023](https://adventofcode.com/2023).
My solutions are written in [Haskell](https://www.haskell.org/).

## Project Structure

The base directory contains a folder for utilities that can be used between days.
The solutions for each day are in a folder named `dayx`.
Each folder contains a main file for part 1 and part 2 and any additional files needed for that day.
The input for each a day should be added in its folder in a file named `input.txt`.

## Running a Solution

To run a solution, run the following command from the base directory
```
./run.bash [day] [part]
```
The first time you run a solution, it may take a few seconds to compile.

## Run All Solutions

To run all solutions at once, run the following command from the base directory
```
./runall.bash
```
This may take a while depending on how long each solution takes to run and how many days need to be compiled.