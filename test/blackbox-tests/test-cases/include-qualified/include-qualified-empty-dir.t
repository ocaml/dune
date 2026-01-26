Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF
  $ mkdir bar
  $ echo hello > bar/some-data.txt

  $ mkdir bar-baz

The directory `bar-baz`, even though not a valid module name, doesn't have any
source files. The library should still compile.

  $ dune build foo.cma

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (rule (with-stdout-to hello.txt (run echo hello)))
  > (library (name bar))
  > EOF

  $ touch bar-baz/hello.ml

  $ dune build ./hello.txt
  File "bar-baz", line 1, characters 0-0:
  Error: "bar-baz" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: bar_baz would be a correct module name
  [1]
