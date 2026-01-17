Advent of FPGA
===========================

(This starter code was adapted from [hardcaml_template_project](https://github.com/janestreet/hardcaml_template_project/tree/with-extensions).)

### Usage

Each solution should be implemented in the `src/dayX.ml` file for the corresponding day `X`. 
Further, each solution module should implement the `Solution` signature defined in `src/soln.ml`.

#### Building

To build the project, use the following command:

```bash
dune build
```
(you may need to run `opam install . --deps-only` or get the necessary dependencies first).

To generate the RTL for a specific solution, run:

```bash
dune exec bin/generate.exe -- day<day_number>
```
(Or just run `dune exec bin/generate.exe` and read the usage.)

#### Testing

Solutions should have corresponding test code in the `test/test_day<X>.ml` file. 
The tests are *expect_tests* that validate the correctness of the solution logic with respect to the Advent of Code inputs (in [`./test/inputs/day<X>_{example,test}.txt`](./test/inputs/)).

```bash
dune runtest
```


### Completed Days:

*Perhaps I was too ambitious with the number of days planned...*

- [x] [Day 1](src/day1.ml)
- [ ] [~~Day 2~~](src/day2.ml)
- [ ] [~~Day 3~~](src/day3.ml)
- [ ] [~~Day 4~~](src/day4.ml)
- [ ] [~~Day 5~~](src/day5.ml)
- [ ] [~~Day 6~~](src/day6.ml)
- [ ] [~~Day 7~~](src/day7.ml)
- [ ] [~~Day 8~~](src/day8.ml)
- [ ] [~~Day 9~~](src/day9.ml)
- [ ] [~~Day 10~~](src/day10.ml)
- [ ] [~~Day 11~~](src/day11.ml)
- [ ] [~~Day 12~~](src/day12.ml)