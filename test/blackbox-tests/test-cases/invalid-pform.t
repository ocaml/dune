When an unknown pform is encountered, a sensible message is printed out.

  $ echo '(lang dune 2.0)' > dune-project
  $ echo '(rule (copy %{unknown} x.ml))' > dune
  $ dune build
  File "dune", line 1, characters 12-22:
  1 | (rule (copy %{unknown} x.ml))
                  ^^^^^^^^^^
  Error: Unknown variable %{unknown}
  [1]

A known variable can also be unavailable in the current context: the default
context has no findlib toolchain.

  $ make_dune_project 3.0
  $ cat > dune <<'EOF'
  > (rule (alias toolchain) (action (echo %{toolchains})))
  > EOF
  $ dune build @toolchain
  File "dune", line 1, characters 38-51:
  1 | (rule (alias toolchain) (action (echo %{toolchains})))
                                            ^^^^^^^^^^^^^
  Error: No toolchain defined for this context
  [1]
