the name field can be omitted for libraries when public_name is present
  $ dune build --root no-name-lib
  Entering directory 'no-name-lib'

this isn't possible for older syntax <= (1, 0)
  $ dune build --root no-name-lib-syntax-1-0
  Entering directory 'no-name-lib-syntax-1-0'
  File "dune", line 1, characters 22-25:
  1 | (library (public_name foo))
                            ^^^
  Error: name field cannot be omitted before version 1.1 of the dune language
  [1]

executable(s) stanza works the same way

  $ dune build --root no-name-exes
  Entering directory 'no-name-exes'

  $ dune build --root no-name-exes-syntax-1-0
  Entering directory 'no-name-exes-syntax-1-0'
  File "dune", line 1, characters 0-36:
  1 | (executables (public_names foo bar))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: names field may not be omitted before dune version 1.1
  [1]

there's only a public name but it's invalid as a name

  $ dune build --root public-name-invalid-name
  Entering directory 'public-name-invalid-name'
  File "dune", line 1, characters 22-28:
  1 | (library (public_name c.find))
                            ^^^^^^
  Error: Invalid library name.
  Public library names don't have this restriction. You can either change this
  public name to be a valid library name or add a "name" field with a valid
  library name.
  Hint: Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: c_find would be a correct library name
  [1]

there's only a public name which is invalid, and the library is unwrapped
this is an error since 2.0.0, it was only a warning before

  $ dune build --root public-name-invalid-wrapped-false
  Entering directory 'public-name-invalid-wrapped-false'
  File "dune", line 3, characters 14-21:
  3 |  (public_name foo.bar))
                    ^^^^^^^
  Error: Invalid library name.
  Public library names don't have this restriction. You can either change this
  public name to be a valid library name or add a "name" field with a valid
  library name.
  Hint: Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: foo_bar would be a correct library name
  [1]

lib: invalid name
  $ dune exec ./bar.exe --root name-invalid-lib
  Entering directory 'name-invalid-lib'
  File "dune", line 3, characters 7-14:
  3 |  (name foo.bar)
             ^^^^^^^
  Error: "foo.bar" is an invalid library name.
  Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: foo_bar would be a correct library name
  [1]


exe: invalid name
  $ dune build --root name-invalid-exe
  Entering directory 'name-invalid-exe'
  File "dune", line 2, characters 7-10:
  2 |  (name a.b))
             ^^^
  Error: Module "A.b" doesn't exist.
  [1]

exe: invalid public-name
  $ dune build --root public-name-invalid-exe
  Entering directory 'public-name-invalid-exe'
  File "dune", line 2, characters 14-17:
  2 |  (public_name a.b))
                    ^^^
  Error: Module "A.b" doesn't exist.
  [1]
