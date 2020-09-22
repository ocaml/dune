Since dune 2.8 libraries `enabled_if` can use the `%{context_name}` variable.

dune < 2.8
  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ dune build bar
  File "dune", line 8, characters 18-31:
  8 |  (enabled_if (= %{context_name} "not-the-context-name")))
                        ^^^^^^^^^^^^^
  Error: This variable is only available since version 2.8 of the dune
  language. Please update your dune-project file to have (lang dune 2.8).
  [1]


dune >= 2.8
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

+ Print the context
  $ dune build @print_context
  default

+ Not the actual context
  $ dune exec ./bar_exe.exe
  File "dune", line 18, characters 12-15:
  18 |  (libraries bar))
                   ^^^
  Error: Library "bar" in _build/default is hidden (unsatisfied 'enabled_if').
  Hint: try:
    dune external-lib-deps --missing ./bar_exe.exe
  [1]

+ The actual context
  $ dune exec ./foo_exe.exe
  bar
