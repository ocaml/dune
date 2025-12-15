Exercise the behaviour of "dune ocaml doc" when the lockdir
doesn't contain a lockfile for the "ocaml" package.

  $ . ../helpers.sh

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: No compiler declared in the lockfile
  Hint: Try running 'dune build'
  [1]
