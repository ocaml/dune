A `(toplevel ...)` stanza with `(preprocess (pps hello_ppx))` —
where `hello_ppx` declares `(ppx_runtime_libraries hello)` — must
pull `hello` into the toplevel's compile-rule dep set. Pins that
the toplevel's compile rule has a glob over `hello`'s byte objdir,
sourced from the ppx runtime declaration alone — never named in
source.

  $ make_dune_project 3.24

`hello` is the ppx runtime lib and `hello_ppx` is a no-op
ppx_rewriter declaring it as a runtime lib. The helper uses the
stub-driver pattern from `ppx-runtime-libraries.t`.

  $ make_hello_ppx_runtime_fixture

The toplevel preprocesses with `hello_ppx`:

  $ mkdir tp
  $ cat > tp/dune <<EOF
  > (toplevel (name tt) (preprocess (pps hello_ppx)))
  > EOF

  $ dune build tp/tt.exe

The toplevel's compile rule has a glob over `hello`'s byte objdir
— sourced from `hello_ppx`'s `ppx_runtime_libraries`, with no
syntactic reference to `Hello` in toplevel sources. (The
toplevel's synthesised main module isn't user-named, so
`%{cmo:...}` won't resolve; locate the `.cmo` under the stanza
dir.)

  $ CMO=$(find _build/default/tp -name '*.cmo' | head -1)
  $ dune rules --root . --format=json --deps "$CMO" > deps.json
  $ jq_dune -r '.[] | depsGlobs
  >   | select(.dir | endswith("hello/.hello.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/hello/.hello.objs/byte *.cmi
