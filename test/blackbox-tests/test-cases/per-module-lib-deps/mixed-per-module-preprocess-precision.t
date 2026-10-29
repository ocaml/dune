Precision regression guard for per-pair tight-eligibility on an
unwrapped library with mixed per-module preprocessing. When a
consumer references only the default-pp module of a mixed-pp lib,
the per-module narrowing excludes the staged-pps module from the
consumer's compile-rule deps. Asserted by giving the staged-pps
module an unresolvable identifier; if the narrowing regressed,
dune would compile that module and fail.

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

The consumer's compile rule tracks only `a.cmi` from `mylib`'s
byte objdir — narrowing dropped the wide glob, so `b.cmi` is not
materialised and `b.ml` is not compiled.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/consumer}' > deps.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("mylib/.mylib.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("mylib/.mylib.objs/byte/a.cmi"))' < deps.json
  _build/default/mylib/.mylib.objs/byte/a.cmi
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("mylib/.mylib.objs/byte/b.cmi"))' < deps.json

Build only the consumer's `.cmo` (compile rule, not link). The
narrowed dep set means `b.ml` is not compiled.

  $ dune build '%{cmo:consumer/consumer}'
