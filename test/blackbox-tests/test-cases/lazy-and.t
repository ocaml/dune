Running `dune fmt` when a dependency is not available should not crash this way.
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat > foo.ml << EOF
  > let () =                          Fun.id ()
  > EOF

  $ touch .ocamlformat

  $ cat > dune << EOF
  > (test
  >  (name foo)
  >  (enabled_if
  >   (and %{lib-available:nope} (= %{version:nope} 9.9.9))))
  > EOF

  $ dune fmt
  File "dune", line 4, characters 32-47:
  4 |   (and %{lib-available:nope} (= %{version:nope} 9.9.9))))
                                      ^^^^^^^^^^^^^^^
  Error: Package "nope" doesn't exist in the current project and isn't
  installed either.
  [1]

Note that this also fails for `dune build`, but maybe that's expected?
  $ dune build
  File "dune", line 4, characters 32-47:
  4 |   (and %{lib-available:nope} (= %{version:nope} 9.9.9))))
                                      ^^^^^^^^^^^^^^^
  Error: Package "nope" doesn't exist in the current project and isn't
  installed either.
  [1]

The file is not formatted.
  $ cat foo.ml
  let () =                          Fun.id ()

With a normal dune file this works as intended
  $ rm dune
  $ cat > dune << EOF
  > (test
  >  (name foo))
  > EOF

  $ dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]

  $ cat foo.ml
  let () = Fun.id ()
