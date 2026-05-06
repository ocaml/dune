A library declared as [(kind ppx_rewriter) (ppx_runtime_libraries
baz)] declares libraries the consumer needs at compile time
*after* preprocessing. The runtime libraries' modules are
referenced by ppx-rewritten output, so ocamldep on the consumer's
source cannot reason about which of them are referenced — the
next ppx invocation may use any module of [baz].

The compile rule for any consumer of such a ppx must therefore
have a dep that covers every cmi in the runtime library's objdir.
A glob over the objdir is the natural shape; this test pins that
shape, guarding against any future inter-library dep handling
that narrows the recorded deps for ppx_runtime_libraries-induced
libraries.

  $ make_dune_project 3.24

[baz] is a regular unwrapped library that the ppx's expansion
would reference at runtime.

  $ mkdir baz
  $ cat > baz/dune <<EOF
  > (library (name baz) (wrapped false))
  > EOF
  $ cat > baz/baz_mod.ml <<EOF
  > let value = 0
  > EOF

[bar] is a [ppx_rewriter] declaring [baz] as a runtime library.
The [ppx.driver] entry lets dune treat [bar] as a [pps] target
without ppxlib; the driver itself is a no-op stub that satisfies
dune's invocation contract enough for rule generation.

  $ mkdir bar
  $ cat > bar/dune <<EOF
  > (library
  >  (name bar)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries baz)
  >  (ppx.driver (main Bar.main)))
  > EOF
  $ cat > bar/bar.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.Set (ref false), "")
  >     ]
  >   in
  >   let anon _ = () in
  >   Arg.parse (Arg.align args) anon "";
  >   close_out (open_out !out)
  > EOF

[foo] uses [bar] as a preprocessor. [foo.ml] does not mention
[Baz_mod] textually — the ppx's runtime references would only
appear post-pp. Without [(libraries ...)], [foo]'s
[requires_compile] is populated entirely by [add_pp_runtime_deps]
in [src/dune_rules/lib.ml]: [bar] (the pps) plus its runtime
closure ([baz]).

  $ mkdir foo
  $ cat > foo/dune <<EOF
  > (library (name foo) (wrapped false) (preprocess (pps bar)))
  > EOF
  $ cat > foo/foo.ml <<EOF
  > let _ = ()
  > EOF

The compile rule's recorded deps for [foo.cmi] must cover every
cmi in [baz/.baz.objs/byte/]. Without that, a change to any cmi
in [baz] would not invalidate [foo.cmi]'s rule and an incremental
rebuild could silently use a stale [foo.cmi]. The [-I
baz/.baz.objs/byte] flag is already on the compile command line
via [requires_compile] (so the initial build succeeds regardless),
but only the rule's recorded deps drive incremental correctness.

  $ dune rules --root . --format=json --deps _build/default/foo/.foo.objs/byte/foo.cmi |
  > jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("baz/.baz.objs/byte"))
  >   | "\(.dir) \(.predicate)"' | sort -u
  _build/default/baz/.baz.objs/byte *.cmi
