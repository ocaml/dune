Current directory (and also current env) can affect an expander,
and expander is in turn used to expand things in env.

Given that we only have a few string_with_vars in env, a reasonable
example is hard to come up with, but here is a contrived one.

  $ echo '(lang dune 2.0)' > dune-project
  $ cat > dune <<EOF
  > (env
  >  (_
  >   (ocamlc_flags "%{inline_tests}")))
  > EOF

  $ dune printenv --root . . --field ocamlc_flags
  (ocamlc_flags (enabled))


  $ echo '(lang dune 2.0)' > dune-project
  $ cat > dune <<EOF
  > (env
  >  (_
  >   (ocamlc_flags "%{inline_tests}")
  >   (inline_tests ignored)))
  > EOF

  $ dune printenv --root . . --field ocamlc_flags
  (ocamlc_flags (ignored))
