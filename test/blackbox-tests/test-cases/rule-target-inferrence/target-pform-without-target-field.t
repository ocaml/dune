When a rule uses %{target} but omits the (target ...) field, dune should
produce a clear error message pointing the user at the missing field. See
https://github.com/ocaml/dune/issues/12439.

  $ make_dune_project 3.24
  $ cat > dune <<EOF
  > (rule
  >  (action
  >   (write-file %{target} hello)))
  > EOF

  $ dune build
  File "dune", line 3, characters 14-23:
  3 |   (write-file %{target} hello)))
                    ^^^^^^^^^
  Error: You cannot use %{target} unless the rule has a (target ...) or
  (targets ...) field.
  [1]
