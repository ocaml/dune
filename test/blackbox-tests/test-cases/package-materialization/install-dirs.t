CR-soon Alizter: split this test into separate regression checks for workspace
directory installation, direct installed-package error locations, and
inferred-package directory expansion.

A workspace package can install a built directory target via
`(install (dirs ...))`. A consumer rule with `(deps (package ...))` must
materialize that directory without crashing.

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

A rule depending on the workspace package must materialize its directory
install entry without treating it as a file target.

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

Install the package outside the workspace and consume it from a separate
project. A Unix socket in an installed subdirectory will make recursive
dependency expansion fail.

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

Because `dirpkg` is explicitly requested, the error points to
`(package dirpkg)`.

  $ dune build --root consumer out
  Entering directory 'consumer'
  File "dune", line 3, characters 16-22:
  3 |  (deps (package dirpkg))
                      ^^^^^^
  Error: Encountered a special file while expanding dependency.
  Leaving directory 'consumer'
  [1]

Now request workspace package `consumer`, whose library requires installed
library `dirpkg`, alongside an unrelated package. Library-induced package
expansion must include `dirpkg`'s non-library installed directory and
encounter the same socket.

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

The failure confirms that the inferred package's installed directory is read.
Its dependency location is lost: the diagnostic identifies only the alias,
not `(package consumer)` as the root that pulled in `dirpkg`.

  $ dune build --root consumer @check
  Entering directory 'consumer'
  Error: Encountered a special file while expanding dependency.
  -> required by alias check in dune:2
  Leaving directory 'consumer'
  [1]
