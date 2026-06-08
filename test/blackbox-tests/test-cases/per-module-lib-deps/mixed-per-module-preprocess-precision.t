Reproduction: today, building only the consumer's `.cmo` of an
executable that depends on `mylib` (where `mylib` has two modules,
`a` default-pp and `b` `(staged_pps ...)`) fails on a compile error
in `mylib/b.ml` — even though the consumer references only `A`.
The cctx-wide `.cmi` glob over `mylib`'s objdir pulls `b.cmi` into
the consumer's compile rule, which forces dune to compile `b.ml`,
and `b.ml` contains an unresolvable identifier.

Companion to `mixed-per-module-preprocess.t` (the soundness sibling).

  $ make_dune_project 3.24

A no-op staged ppx (identical to the soundness reproducer).

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name ppx_noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx_noop.main)))
  > EOF
  $ cat > ppx/ppx_noop.ml <<EOF
  > let main () =
  >   let n = Array.length Sys.argv in
  >   if n < 4 || Sys.argv.(1) <> "--as-ppx" then assert false;
  >   let input = Sys.argv.(n - 2) in
  >   let output = Sys.argv.(n - 1) in
  >   Filename.quote_command "cp" [input; output]
  >   |> Sys.command
  >   |> exit
  > EOF

`mylib`: `a` uses default preprocessing (Some-entry); `b` uses
`(staged_pps ...)` (None-entry). `a`'s source is independent of `b`;
`b.ml` contains an unresolvable identifier so any attempt to compile
it will fail.

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped false)
  >  (preprocess (per_module ((staged_pps ppx_noop) b))))
  > EOF
  $ cat > mylib/a.ml <<EOF
  > let answer = 42
  > EOF
  $ cat > mylib/b.ml <<EOF
  > let bar = no_such_thing
  > EOF

`consumer` references only `A`:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries mylib))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = print_int A.answer
  > EOF

The consumer's compile rule tracks the wide glob over `mylib`'s
byte objdir — which materialises `b.cmi` and so forces dune to
compile `b.ml`.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/consumer}' > deps.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("mylib/.mylib.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("mylib/.mylib.objs/byte/a.cmi"))' < deps.json
  _build/default/mylib/.mylib.objs/byte/a.cmi
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("mylib/.mylib.objs/byte/b.cmi"))' < deps.json

Build only the consumer's `.cmo` (compile rule, not link). Today,
dune attempts to compile `b.ml` to produce `b.cmi` and fails on
the unresolvable identifier.

  $ dune build '%{cmo:consumer/consumer}'
