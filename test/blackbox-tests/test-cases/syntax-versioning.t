  $ echo '(lang dune 1.0)' > dune-project

  $ echo '(jbuild_version 1)' > dune
  $ dune build
  File "dune", line 1, characters 0-18:
  1 | (jbuild_version 1)
      ^^^^^^^^^^^^^^^^^^
  Error: 'jbuild_version' was deleted in version 1.0 of the dune language.
  [1]
  $ rm -f dune

Since 3.0.0, jbuild files are plain ignored:

  $ echo 'random stuff' > jbuild
  $ dune build
  $ rm -f jbuild

  $ echo '(executable (name x) (link_executables false))' > dune
  $ dune build
  File "dune", line 1, characters 21-45:
  1 | (executable (name x) (link_executables false))
                           ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'link_executables' was deleted in version 1.0 of the dune language.
  [1]
  $ rm -f dune

  $ touch x
  $ echo '(alias (name default) (deps x) (action (run %{<})))' > dune
  $ dune build
  File "dune", line 1, characters 44-48:
  1 | (alias (name default) (deps x) (action (run %{<})))
                                                  ^^^^
  Error: %{<} was deleted in version 1.0 of the dune language.
  Use a named dependency instead:
  
    (deps (:x <dep>) ...)
     ... %{x} ...
  [1]
  $ rm -f dune

  $ echo '(lang dune 1.7)' > dune-project
  $ cat > dune <<EOF
  > (library
  >  (name foo)
  >  (no_keep_locs))
  > EOF
  $ dune build
  File "dune", line 3, characters 1-15:
  3 |  (no_keep_locs))
       ^^^^^^^^^^^^^^
  Warning: no_keep_locs is a no-op. Please delete it.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (no_keep_locs disabled))
  $ rm -f dune
