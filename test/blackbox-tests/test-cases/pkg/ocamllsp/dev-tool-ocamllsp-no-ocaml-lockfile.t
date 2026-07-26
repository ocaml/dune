Exercise the behaviour of "dune tools exec ocamllsp" when the lockdir
doesn't contain a lockfile for the "ocaml" package.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir

  $ dune tools exec ocamllsp
  Error: The tool ocamllsp is not installed.
  Hint: Try running 'dune tools install ocamllsp'
  [1]
