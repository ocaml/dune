Incremental builds with library re-exporting a dependency via module alias.

When library "alias" re-exports library "impl" via (module Impl = Impl),
a consumer that accesses Impl through Alias must be recompiled when
impl.cmi changes. The -opaque flag means soft changes (implementation
only, no cmi change) can safely skip recompilation, but cmi changes
must always trigger it.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

A library where we'll perform the changes:

  $ mkdir impl
  $ cat > impl/dune <<EOF
  > (library (name impl))
  > EOF
  $ cat > impl/impl.ml <<EOF
  > let foo = "initial build"
  > EOF

Another library which exposes an alias to impl:

  $ mkdir alias
  $ cat > alias/dune <<EOF
  > (library (name alias) (libraries impl))
  > EOF
  $ cat > alias/alias.ml <<EOF
  > module Impl = Impl
  > EOF

A binary which depends on Alias to access Impl. An empty unused file
makes this a multi-module executable:

  $ mkdir bin
  $ cat > bin/dune <<EOF
  > (executable (name main) (libraries alias))
  > EOF
  $ cat > bin/main.ml <<EOF
  > let () = print_endline Alias.Impl.foo
  > EOF
  $ touch bin/unused.ml

The first build succeeds:

  $ dune exec ./bin/main.exe
  initial build

Soft update — impl.cmi is NOT modified (only implementation changes).
With -opaque, skipping recompilation of main.ml is correct because
main.ml doesn't depend on impl's implementation, only its interface:

  $ cat > impl/impl.ml <<EOF
  > let foo = "second build, no change to cmi"
  > EOF

  $ dune exec ./bin/main.exe
  second build, no change to cmi

main.cmx is NOT rebuilt (correct — only impl changed, and -opaque
means we don't track impl's implementation):

  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Main"))]'
  []

unused.cmx is also NOT rebuilt (correct — it references nothing):

  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Unused"))]'
  []

Hard update — impl.cmi IS modified (new value added). main.ml must
be recompiled because Alias re-exports Impl and the interface changed:

  $ cat > impl/impl.ml <<EOF
  > let new_value = 42
  > let foo = "third build, forced a cmi update"
  > EOF

  $ dune exec ./bin/main.exe
  third build, forced a cmi update

Main is rebuilt (necessary — impl.cmi changed and main.ml uses
Impl through the Alias re-export):

  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Main"))] | length | . > 0'
  true

Unused is NOT rebuilt (correct — it doesn't reference impl):

  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Unused"))] | length'
  0
