Aliases requested on the workspace do not enter the packages mounted from the
lock directory. Every rule of the locked package below fails, so any alias
that reached it would fail.

  $ make_lockdir
  $ cat > dune-project <<'EOF2'
  > (lang dune 3.25)
  > (package (name main) (allow_empty))
  > EOF2
  $ cat > dune <<'EOF2'
  > (rule
  >  (alias runtest)
  >  (action
  >   (echo "workspace test\n")))
  > EOF2
  $ mkdir _source
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ cat > _source/dune <<'EOF2'
  > (library (public_name foo))
  > (rule (target generated) (action (run false)))
  > (install (section share) (files generated))
  > (rule (alias runtest) (action (run false)))
  >   (rule (alias   badly-formatted) (action (run false)))
  > EOF2
  $ echo 'let x : int = "not an int"' > _source/foo.ml
  $ make_dune_source_lockpkg native foo "$PWD/_source"

  $ dune build
  $ dune build @default @all @check @install @fmt @doc
  $ dune build @runtest
  workspace test
  $ dune runtest

The same aliases requested inside the mount do reach those rules. Formatting
is the exception: a dependency's files are not checked.

  $ dune build @_build/default/.pkg/foo/runtest
  File "_build/default/.pkg/foo/dune", line 4, characters 0-43:
  Command exited with code 1.
  [1]
  $ dune build _build/default/.pkg/foo/generated
  File "_build/default/.pkg/foo/dune", line 2, characters 0-46:
  Command exited with code 1.
  [1]
  $ dune build @_build/default/.pkg/foo/fmt
  $ dune build @_build/default/.pkg/foo/check
  File ".pkg/foo/foo.ml", line 1, characters 14-26:
  1 | let x : int = "not an int"
                    ^^^^^^^^^^^^
  Error: This constant has type string but an expression was expected of type
           int
  [1]
