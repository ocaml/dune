Exercise the behaviour of "dune ocaml doc" when run in a
dune project with no lockdir.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: Unable to load the lockdir for the default build context.
  Hint: Try running 'dune pkg lock'
  [1]
