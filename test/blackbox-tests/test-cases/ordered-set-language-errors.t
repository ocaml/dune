Test the error cases for dune's Ordered Set Language (OSL) DSL.

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

Make a dune file and build the project, using the "flags" field to exercise OSL.
  $ test() {
  >   cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (flags $1))
  > EOF
  > dune build
  > }

  $ test ':include'
  File "dune", line 3, characters 8-16:
  3 |  (flags :include))
              ^^^^^^^^
  Error: Invalid use of :include, should be: (:include <filename>)
  [1]

  $ test '(:include)'
  File "dune", line 3, characters 8-18:
  3 |  (flags (:include)))
              ^^^^^^^^^^
  Error: Not enough arguments for :include
  [1]

  $ test '(:include x)'
  File ".merlin-conf/_unknown_", line 1, characters 0-0:
  Error: No rule found for x
  [1]

  $ test '(:include x y)'
  File "dune", line 3, characters 20-21:
  3 |  (flags (:include x y)))
                          ^
  Error: Too many arguments for :include
  [1]

  $ test '(x y)'
  File "dune", line 3, characters 9-10:
  3 |  (flags (x y)))
               ^
  Error: This atom must be quoted because it is the first element of a list and
  doesn't start with - or:
  [1]

  $ test '(:includ)'
  File "dune", line 3, characters 9-16:
  3 |  (flags (:includ)))
               ^^^^^^^
  Error: undefined symbol :includ
  [1]
