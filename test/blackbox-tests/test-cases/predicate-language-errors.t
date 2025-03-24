Test the error cases for dune's Predicate Language DSL.

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

Make a dune file and build the project, using the "with-accepted-exit-codes"
action to exercise the predicate language.
  $ test() {
  >   cat > dune <<EOF
  > (rule
  >  (action
  >   (with-accepted-exit-codes
  >    $1
  >    (with-stdout-to x.txt
  >     (run echo x)))))
  > EOF
  > dune build x.txt
  > }

  $ test 0

  $ test '(:include x)'
  File "dune", line 4, characters 4-12:
  4 |    (:include x)
          ^^^^^^^^
  Error: :include isn't supported in the predicate language
  [1]

  $ test '(x y)'
  File "dune", line 4, characters 4-5:
  4 |    (x y)
          ^
  Error: This atom must be quoted because it is the first element of a list and
  doesn't start with - or:
  [1]
