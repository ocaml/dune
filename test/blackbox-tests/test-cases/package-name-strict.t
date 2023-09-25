Version check:

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > (package
  >  (name some&name)
  >  (allow_empty))
  > EOF
  $ dune build

Validation:

  $ test() {
  > cat > dune-project << EOF
  > (lang dune 3.11)
  > (package
  >  (name $1)
  >  (allow_empty))
  > EOF
  > dune build
  > }

  $ test 'some&name'
  File "dune-project", line 3, characters 7-16:
  3 |  (name some&name)
             ^^^^^^^^^
  Error: "some&name" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: some_name would be a correct opam package name
  [1]

Leading invalid characters are removed:

  $ test '0test'

When all characters are removed, a valid name is suggested:

  $ test '0'
  File "dune-project", line 3, characters 7-8:
  3 |  (name 0)
             ^
  Error: "0" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: p0 would be a correct opam package name
  [1]

A package name can start with a number:

  $ test 0install

But it needs at least a letter:

  $ test 0-9
  File "dune-project", line 3, characters 7-10:
  3 |  (name 0-9)
             ^^^
  Error: "0-9" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: p0-9 would be a correct opam package name
  [1]
