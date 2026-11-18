lib: invalid name
  $ dune exec ./bar.exe 
  File "dune", line 3, characters 7-14:
  3 |  (name foo.bar)
             ^^^^^^^
  Error: "foo.bar" is an invalid library name.
  Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: foo_bar would be a correct library name
  [1]

Invalid final characters currently reach module-name validation instead of
being rejected as library names.

  $ rm dune
  $ cat > dune <<'EOF'
  > (library
  >  (modules foo)
  >  (name foo-)
  >  (wrapped false))
  > EOF
  $ dune build
  File "dune", line 3, characters 7-11:
  3 |  (name foo-)
             ^^^^
  Error: "foo-" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: foo_ would be a correct module name
  [1]

