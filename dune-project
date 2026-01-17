(lang dune 3.17)

(name advent_of_fpga)

(generate_opam_files true)

(package
 (name advent_of_fpga)
 (synopsis
  "A collection of Advent of Code solutions implemented with Hardcaml")
 (description
  "A collection of Advent of Code solutions implemented with Hardcaml")
 (depends
  ocaml
  dune
  core
  hardcaml
  hardcaml_test_harness
  hardcaml_circuits
  hardcaml_waveterm
  re
  ppx_jane
  ppx_hardcaml
  core_unix
  jane_rope)
 (tags
  ("hardware" "advent of code")))
