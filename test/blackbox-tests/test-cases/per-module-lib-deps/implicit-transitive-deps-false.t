Regression: under [(implicit_transitive_deps false)], a transitive
link-only library must not invalidate the consumer when the consumer
cannot see it through [-I] or [-H].

[libA] is a transitive dep of [main]: [main] depends on [libB]
which declares [libA]. With [(implicit_transitive_deps false)],
[libA]'s modules are not on [main]'s [-I] path; [main]'s source
cannot reference [ModA] at all. The per-module dependency
computation in #14116 must therefore not surface [libA] as a
compile-rule dep — an earlier iteration of that PR did, via a
glob over the lib's objdir, causing spurious [Main] recompiles
when [modA] changed.

Reported by @nojb in
https://github.com/ocaml/dune/pull/14116#issuecomment-4323883194.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (implicit_transitive_deps false)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA))
  > (library
  >  (name libB)
  >  (wrapped false)
  >  (modules modB)
  >  (libraries libA))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries libB))
  > EOF

  $ cat > modA.ml <<EOF
  > let x = 42
  > EOF

  $ cat > modB.ml <<EOF
  > let x = 42
  > EOF

  $ cat > main.ml <<EOF
  > let _ = ModB.x
  > EOF

  $ dune build ./main.exe

Edit [modA]. [Main] doesn't reference [ModA] and the compiler
cannot see [libA] under [(implicit_transitive_deps false)], so
[Main] must not be recompiled (the executable may still relink
because [modA.cmx] changes, but [Main.cmo] / [Main.cmx] must
not):

  $ cat > modA.ml <<EOF
  > let x = 43
  > EOF
  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))]'
  []
