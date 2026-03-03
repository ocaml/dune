`dune describe workspace` should not fail on missing external libraries.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name alive)
  >  (modules alive))
  > 
  > (library
  >  (name foo)
  >  (modules foo)
  >  (libraries does_not_exist_gh12997))
  > EOF

  $ cat > alive.ml << EOF
  > let x = 1
  > EOF

  $ cat > foo.ml << EOF
  > let x = 1
  > EOF

  $ dune describe workspace --sanitize-for-tests
  File "dune", line 8, characters 12-34:
  8 |  (libraries does_not_exist_gh12997))
                  ^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "does_not_exist_gh12997" not found.
  -> required by library "foo" in _build/default
  [1]
