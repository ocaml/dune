Reproduction for GH-15578: qualified modules cannot be selected in
`(preprocess (per_module ...))`.

  $ make_dune_project 3.24

  $ mkdir foo
  $ touch foo/bar.ml

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) foo.bar))))
  > EOF

  $ dune build
  File "dune", line 7, characters 30-37:
  7 |      (run cat %{input-file})) foo.bar))))
                                    ^^^^^^^
  Error: "foo.bar" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: foo_bar would be a correct module name
  [1]

Using a slash instead does not work either:

  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name x)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run cat %{input-file})) foo/bar))))
  > EOF

  $ dune build
  File "dune", line 7, characters 30-37:
  7 |      (run cat %{input-file})) foo/bar))))
                                    ^^^^^^^
  Error: "foo/bar" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: foobar would be a correct module name
  [1]
