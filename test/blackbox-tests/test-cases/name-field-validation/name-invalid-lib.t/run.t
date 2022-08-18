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

