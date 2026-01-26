Black-box testing: we check that warning 32 (unused-value-declaration) is
enabled by default in the dev profile in Dune version 3.20 but not in 3.21.

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat >main.ml <<EOF
  > let unused = 42
  > EOF

  $ cat >main.mli <<EOF
  > EOF

  $ cat >dune <<EOF
  > (executable (name main))
  > EOF

  $ dune build
  File "main.ml", line 1, characters 4-10:
  1 | let unused = 42
          ^^^^^^
  Error (warning 32 [unused-value-declaration]): unused value unused.
  [1]

Now in 3.21...

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ dune build

Version check for %{dune-warnings}.

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat >dune <<EOF
  > (executable (name main) (flags :standard %{dune-warnings}))
  > EOF

  $ dune build
  File "dune", line 1, characters 41-57:
  1 | (executable (name main) (flags :standard %{dune-warnings}))
                                               ^^^^^^^^^^^^^^^^
  Error: %{dune-warnings} is only available since version 3.21 of the dune
  language. Please update your dune-project file to have (lang dune 3.21).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ dune build
  File "main.ml", line 1, characters 4-10:
  1 | let unused = 42
          ^^^^^^
  Error (warning 32 [unused-value-declaration]): unused value unused.
  [1]

What _is_ %{dune-warnings} ?

  $ cat >dune <<EOF
  > (rule (alias show) (action (echo "%{dune-warnings}\n")))
  > EOF

  $ dune build @show
  -w @1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats

Warnings are still fatal in dev mode.

  $ cat >main.ml <<EOF
  > let f = function 0 -> 1
  > EOF

  $ cat >dune <<EOF
  > (executable (name main))
  > EOF

  $ dune build
  File "main.ml", line 1, characters 8-23:
  1 | let f = function 0 -> 1
              ^^^^^^^^^^^^^^^
  Error (warning 8 [partial-match]): this pattern-matching is not exhaustive.
    Here is an example of a case that is not matched: 1
  [1]
