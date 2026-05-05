A consumer of a library that uses [(preprocess (action ...))] must
still build when the dep's pre-preprocessing source is not valid
OCaml on its own. Concretely: if any code path runs ocamldep on the
raw, pre-[Module.pped] form of the dep's [.ml] (the [Module.t] from
[Dir_contents.modules_of_local_lib], whose [Module.source] points
at the unprocessed [.ml] and whose [Module.pp_flags] is [None]),
ocamldep rejects the source with a syntax error and the build
fails.

The neighbour test [cross-lib-action-preprocess.t] covers the same
scenario with a pass-through preprocessor and plain-OCaml sources,
so a code path that runs ocamldep on the pre-pp form goes
undetected. This test plugs that gap: [pp.exe] strips [#]-prefixed
lines, and [dep/foo.ml] starts with a [#if]/[#endif] block.
Ocamldep on the post-pp output is fine; ocamldep on the pre-pp
input is a syntax error.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > EOF

A trivial preprocessor that drops every line starting with [#].
This mimics cppo's behaviour without needing an external tool: the
[.ml] holds [#if]/[#endif]-style markers; the preprocessor's output
is plain OCaml.

  $ mkdir pp
  $ cat > pp/dune <<EOF
  > (executable (name pp))
  > EOF
  $ cat > pp/pp.ml <<'EOF'
  > let () =
  >   let ic = open_in_bin Sys.argv.(1) in
  >   try
  >     while true do
  >       let line = input_line ic in
  >       if String.length line = 0 || line.[0] <> '#'
  >       then print_endline line
  >     done
  >   with End_of_file -> ()
  > EOF

[dep] is an unwrapped, multi-module library whose modules are
preprocessed. [foo.ml] has a [#]-prefixed directive that ocamldep
cannot parse but the preprocessor strips. [bar.ml] is the second
module: keeping the lib multi-module ensures any future per-module
code path that short-circuits single-module stanzas doesn't mask
the regression this test guards against.

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library
  >  (name dep)
  >  (wrapped false)
  >  (preprocess (action (run %{exe:../pp/pp.exe} %{input-file}))))
  > EOF
  $ cat > dep/foo.ml <<'EOF'
  > #if FAKE_DIRECTIVE
  > let v = 1
  > #endif
  > EOF
  $ cat > dep/bar.ml <<EOF
  > let helper = 42
  > EOF

[consumer] references [Foo] from [dep]. Any cross-library
dependency-tracking code path that resolves [Foo]'s deps against
the dep's pre-pp source would fail here:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = Foo.v
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

The build must succeed. If a future change reintroduces a cross-
library code path that runs ocamldep on the pre-[Module.pped] form
of a foreign-lib module without re-applying that lib's preprocessor,
this test will fail with a syntax error from [dep/foo.ml].

  $ dune build @check

Confirm that [consumer/c.cmi] depends on [dep]'s objdir for cmis,
and not on any path that names the pre-pp source. The dep is
registered as a [*.cmi] glob over the dep's objdir; a future
regression that switched to a per-file dep on the wrong basename
(e.g. [dep/.dep.objs/byte/foo.pp.cmi] instead of [foo.cmi]) would
surface here as a different recorded dep:

  $ dune rules --root . --format=json --deps _build/default/consumer/.consumer.objs/byte/c.cmi |
  > jq -r 'include "dune"; .[] | depsGlobPredicates'
  *.cmi
