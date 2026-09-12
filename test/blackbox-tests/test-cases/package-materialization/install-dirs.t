Regression test: when a package installs a built directory target via
`(install (dirs ...))`, a consumer rule with `(deps (package ...))` must
not crash. The layout has to symlink the directory entry with
`Action_builder.symlink_dir`, not plain `symlink`.

  $ make_dune_project_with_package 3.24 dirpkg

  $ mkdir src
  $ cat >src/dune <<EOF
  > (library (public_name dirpkg))
  > (rule
  >  (target (dir generated))
  >  (action
  >   (progn (system "mkdir %{target}")
  >          (system "echo top > %{target}/a.txt")
  >          (system "mkdir %{target}/sub")
  >          (system "echo nested > %{target}/sub/b.txt"))))
  > (install (section share) (package dirpkg) (dirs generated))
  > EOF
  $ cat >src/dirpkg.ml <<'EOF'
  > let x = 1
  > EOF

A rule that depends on the package must build without crashing. Without
the kind-aware dispatch, the layout's `share/dirpkg/generated` symlink
would be created as a file pointing at a directory; the action then asks
for `generated` as a file and the engine reports "No rule found".

  $ cat >dune <<EOF
  > (rule
  >  (deps (package dirpkg))
  >  (action (with-stdout-to out (echo ok))))
  > EOF

  $ dune build out
  $ cat _build/default/out
  ok

Spot-check that the installed directory is reachable as a path via
`%{pkg:...}`:

  $ cat >dune <<EOF
  > (rule
  >  (alias check-dir)
  >  (deps (package dirpkg))
  >  (action (bash "cat %{pkg:dirpkg:share:generated}/a.txt")))
  > EOF

  $ dune build @check-dir
  top

Errors while reading an installed directory should point to the package
dependency that caused it to be read.

  $ dune build @install
  $ dune install --prefix "$PWD/prefix"
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ mkdir consumer
  $ echo '(lang dune 3.24)' >consumer/dune-project
  $ cat >consumer/dune <<'EOF'
  > (rule
  >  (target out)
  >  (deps (package dirpkg))
  >  (action (write-file %{target} ok)))
  > EOF
  $ dune build --root consumer out
  $ dune_cmd mksocket prefix/share/dirpkg/generated/sub/socket

The error points to `(package dirpkg)`.

  $ dune build --root consumer out
  Entering directory 'consumer'
  File "dune", line 3, characters 16-22:
  3 |  (deps (package dirpkg))
                      ^^^^^^
  Error: Encountered a special file while expanding dependency.
  Leaving directory 'consumer'
  [1]

The same error can arise from a transitive package dependency. Use an alias
to exercise unnamed dependencies as well, and an unrelated package to check
that the error is attributed to the root that actually requires dirpkg.

  $ cat >>consumer/dune-project <<'EOF'
  > (package (name unrelated) (allow_empty))
  > (package (name consumer))
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (library (public_name consumer) (libraries dirpkg))
  > (alias
  >  (name check)
  >  (deps (package unrelated) (package consumer)))
  > EOF
  $ echo 'let x = Dirpkg.x' >consumer/consumer.ml

The error points to `(package consumer)`.

  $ dune build --root consumer @check
  Entering directory 'consumer'
  Error: Encountered a special file while expanding dependency.
  -> required by alias check in dune:2
  Leaving directory 'consumer'
  [1]
