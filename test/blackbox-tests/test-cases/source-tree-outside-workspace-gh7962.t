Repro for #7962: a rule has a dependency on a `(source_tree)` outside the
workspace. This blows up loudly but should not - we should probably detect that
the path escapes the build directory and error out or handle that as an
external directory.

  $ mkdir proj ext

  $ cat > proj/dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > proj/dune << EOF
  > (alias
  >  (name runtest)
  >  (deps (source_tree ../ext)))
  > EOF

  $ (cd proj; dune runtest)
  File "dune", line 3, characters 20-26:
  3 |  (deps (source_tree ../ext)))
                          ^^^^^^
  Error: path cannot escape the context root
  [1]
