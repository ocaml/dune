Reproducer for the soundness bug Copilot flagged in
[Lib_index.create]: an unwrapped lib's [tight_eligible]
membership is set on "any entry has [Some _]", which under
per-module preprocessing can be true while *some* of the lib's
entries have [m_opt = None]. The classification fold then
silently drops the [None] entries' modules from the consumer's
compile-rule deps when those modules are reached via
[cross_lib_tight_set]'s BFS expansion.

Setup. [mylib] is unwrapped with two modules. [a] uses default
(no preprocessing) — entry [(a, mylib, Some _)]. [b] uses
[(staged_pps ...)] — entry [(b, mylib, None)], since no
[.pp.ml] is statically known for staged pps.
[a]'s source references [B], so [cross_lib_tight_set]'s BFS
walks from [a]'s ocamldep into [B]. The consumer references
only [A], so [wrapped_libs_referenced] (which is keyed off the
consumer's direct ocamldep, [referenced]) does NOT add [mylib]
to [must_glob_set]. The mixed-entry bug therefore reaches the
classification fold via [tight_set], and the fold drops [B]
because [tight_modules_per_lib] skips its [None] entry.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

A no-op staged ppx, modeled on
test/blackbox-tests/test-cases/staged-pps-relative-directory-gh8158.t.
The driver copies its input verbatim, so the staged stage's
[.pp.ml] equals the input source.

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
  >   if n < 2 then assert false;
  >   let input = Sys.argv.(n - 2) in
  >   let output = Sys.argv.(n - 1) in
  >   Filename.quote_command "cp" [input; output]
  >   |> Sys.command
  >   |> exit
  > EOF

[mylib] is unwrapped with two modules; only [b] is staged.
[a]'s interface mentions [B.t], so [cross_lib_tight_set]'s BFS
walks from [a]'s ocamldep into [b]'s name AND the consumer's
own compile genuinely loads [b.cmi] (otherwise the bug wouldn't
manifest at the consumer's compile site).

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped false)
  >  (preprocess (per_module ((staged_pps ppx_noop) b))))
  > EOF
  $ cat > mylib/a.mli <<EOF
  > val identity : B.t -> B.t
  > EOF
  $ cat > mylib/a.ml <<EOF
  > let identity (x : B.t) = x
  > EOF
  $ cat > mylib/b.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

[consumer] references only [A] in source — never names [B] —
but its call to [A.identity] forces ocamlc to load [B.cmi] to
resolve [B.t].

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries mylib))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = A.identity 0
  > EOF

Sandboxed build forces the missing-dep bug to surface
deterministically: the sandbox is populated only from the
compile-rule's declared deps, so a missing dep on
[mylib/.mylib.objs/byte/b.cmi] fails the build with a
"no such file" error rather than a silent race.

  $ dune build --sandbox=copy consumer/consumer.exe
