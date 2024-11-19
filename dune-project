(lang dune 3.17)

(name advent_of_code)

(generate_opam_files true)

(source
 (github atomicptr/advent-of-code-ml))

(maintainers "Christopher Kaster <me@atomicptr.de>")

(authors "Christopher Kaster <me@atomicptr.de>")

(license GPL-3.0)

(package
 (name advent_of_code)
 (synopsis "Advent of Code implemented in OCaml")
 (description "Advent of Code implemented in OCaml")
 (depends ocaml dune ocamlformat alcotest CamelCase)
 (tags ("advent-of-code")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(pin
 (package
  (name CamelCase))
 (url "git+https://github.com/atomicptr/CamelCase"))
