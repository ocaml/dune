Exercise the behaviour of "dune tools exec ocamllsp" when run in a
dune project with no lockdir.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ dune tools exec ocamllsp
  Error: The tool ocamllsp is not installed.
  Hint: Try running 'dune tools install ocamllsp'
  [1]
