Running `dune fmt` when a dependency is not available should not crash this way.
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > EOF

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
