  $ dune exec ./bar.exe
  File "dune", line 3, characters 7-14:
  3 |  (name foo.bar)
             ^^^^^^^
  Warning: Invalid library name.
  This is temporary allowed for libraries with (wrapped false).
  It will not be supported in the future. Please choose a valid name field.
  Hint: library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'
  foo
