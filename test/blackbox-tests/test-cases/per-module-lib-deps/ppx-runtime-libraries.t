A library declared as [(kind ppx_rewriter) (ppx_runtime_libraries
runtime_dep)] declares libraries the consumer needs at compile time
*after* preprocessing. The runtime libraries' modules are
referenced by ppx-rewritten output, so ocamldep on the consumer's
source cannot reason about which of them are referenced — the
next ppx invocation may use any module of [runtime_dep].

The compile rule for any consumer of such a ppx must therefore
have a dep that covers every cmi in the runtime library's objdir.
A glob over the objdir is the natural shape; this test pins that
shape, guarding against any future inter-library dep handling
that narrows the recorded deps for ppx_runtime_libraries-induced
libraries.

  $ make_dune_project 3.24

[runtime_dep] is a regular library that the ppx's expansion would
reference at runtime.

  $ mkdir runtime_dep
  $ cat > runtime_dep/dune <<EOF
  > (library (name runtime_dep))
  > EOF
  $ cat > runtime_dep/runtime_dep_mod.ml <<EOF
  > let value = 0
  > EOF

[preprocessor] is a [ppx_rewriter] declaring [runtime_dep] as a
runtime library. The [ppx.driver] entry lets dune treat
[preprocessor] as a [pps] target without ppxlib; the driver
itself is a no-op stub that satisfies dune's invocation contract
enough for rule generation.

  $ mkdir preprocessor
  $ cat > preprocessor/dune <<EOF
  > (library
  >  (name preprocessor)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries runtime_dep)
  >  (ppx.driver (main Preprocessor.main)))
  > EOF
  $ cat > preprocessor/preprocessor.ml <<EOF
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

[user] applies [preprocessor] as a ppx. [user.ml] does not
mention [Runtime_dep_mod] textually — the ppx's runtime
references would only appear post-pp. Without [(libraries ...)],
[user]'s [requires_compile] is populated entirely by
[add_pp_runtime_deps] in [src/dune_rules/lib.ml]: [preprocessor]
(the pps) plus its runtime closure ([runtime_dep]).

  $ mkdir user
  $ cat > user/dune <<EOF
  > (library (name user) (preprocess (pps preprocessor)))
  > EOF
  $ cat > user/user.ml <<EOF
  > let _ = ()
  > EOF

The compile rule for [user.cmi] must list every cmi in
[runtime_dep/.runtime_dep.objs/byte/] as a dep. Without that
coverage, a change to any of [runtime_dep]'s cmis would not
invalidate [user.cmi], and the next build could silently use a
stale artefact.

The [-I runtime_dep/.runtime_dep.objs/byte] flag is already on
the compile command line via [requires_compile], so the initial
build succeeds. But the flag does not cause rebuilds; only the
recorded deps do.

  $ dune rules --root . --format=json --deps _build/default/user/.user.objs/byte/user.cmi |
  > jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("runtime_dep/.runtime_dep.objs/byte"))
  >   | .dir + " " + .predicate'
  _build/default/runtime_dep/.runtime_dep.objs/byte *.cmi
