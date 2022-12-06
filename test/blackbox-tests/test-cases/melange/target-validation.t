Validation of target field in melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

Target should not be empty

  $ lib=foo
  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target "")
  >  (libraries $foo)
  >  (module_system es6))
  > EOF

  $ dune build
  File "dune", line 5, characters 9-11:
  5 |  (target "")
               ^^
  Error: The field target can not be empty
  [1]

Target should not try to descend into subdirectories

  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target foo/bar)
  >  (libraries $foo)
  >  (module_system es6))
  > EOF

  $ dune build
  File "dune", line 5, characters 9-16:
  5 |  (target foo/bar)
               ^^^^^^^
  Error: The field target must use simple names and can not include paths to
  other folders. To emit JavaScript files in another folder, move the
  `melange.emit` stanza to that folder
  [1]

Target should not try to escape into parent directories

  $ rm dune
  $ mkdir bar
  $ mkdir foo
  $ cat > bar/dune <<EOF
  > (library
  >  (name bar)
  >  (modes melange))
  > EOF

  $ cat > foo/dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target ../bar)
  >  (libraries $foo)
  >  (module_system es6))
  > EOF

  $ dune build
  File "foo/dune", line 5, characters 9-15:
  5 |  (target ../bar)
               ^^^^^^
  Error: The field target must use simple names and can not include paths to
  other folders. To emit JavaScript files in another folder, move the
  `melange.emit` stanza to that folder
  [1]
