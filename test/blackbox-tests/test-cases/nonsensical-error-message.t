Nonsensical error message

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name bar)
  >  (libraries (abc123 foo)))
  > EOF

  $ dune build
  File "dune", line 3, characters 12-24:
  3 |  (libraries (abc123 foo)))
                  ^^^^^^^^^^^^
  Error: This syntax is available only when oxcaml is enabled in the
  dune-project file. You must enable it using (using oxcaml 0.1) in your
  dune-project file.
  Note however that oxcaml is experimental and might change without notice in
  the future.
  [1]
