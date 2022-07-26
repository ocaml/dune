  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF
  $ cat > dune << EOF
  > (library (name 03))
  > EOF
  $ dune build
  File "dune", line 1, characters 15-17:
  1 | (library (name 03))
                     ^^
  Error: "03" is an invalid module name.
  Module names must be non-empty and composed only of the following characters:
  'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: M03 would be a correct module name
  [1]
