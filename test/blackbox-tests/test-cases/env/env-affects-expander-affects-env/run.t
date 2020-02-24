Current directory (and also current env, even though that's not demonstrated here)
can affect an expander, and expander is in turn used to expand things in env.

Given that we only have a few string_with_vars in env, a reasonable
example is hard to come up with, but here's a contrived one.

  $ echo '(lang dune 2.0)' > dune-project
  $ cat > dune <<EOF
  > (library
  >  (name whatever)
  >  (inline_tests)
  >  (libraries ))
  > 
  > (env
  >  (_
  >   (ocamlc_flags "%{inline_tests}")))
  > EOF

  $ dune printenv --root . . | grep ocamlc_flags
    (ocamlc_flags (enabled))
