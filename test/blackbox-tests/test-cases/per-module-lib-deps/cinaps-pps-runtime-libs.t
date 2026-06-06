A `(cinaps ...)` stanza with `(preprocess (pps hello_ppx))` —
where `hello_ppx` declares `(ppx_runtime_libraries hello)` — must
pull `hello` into the cinaps exe's compile-rule dep set. Pins
that the compile rule has a glob over `hello`'s byte objdir,
sourced from the ppx runtime declaration alone — never named in
source.

  $ make_cinaps_project 3.24 1.0

`hello` is the ppx runtime lib and `hello_ppx` is a no-op
ppx_rewriter declaring it as a runtime lib. The helper uses the
stub-driver pattern from `ppx-runtime-libraries.t`.

  $ make_hello_ppx_runtime_fixture

The cinaps stanza preprocesses with `hello_ppx`:

  $ mkdir cn
  $ cat > cn/dune <<EOF
  > (cinaps (files *.ml) (preprocess (pps hello_ppx)))
  > EOF
  $ cat > cn/probe.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @cinaps

The cinaps exe's compile rule has a glob over `hello`'s byte
objdir — sourced from `hello_ppx`'s `ppx_runtime_libraries`, with
no syntactic reference to `Hello` in cinaps sources. (The cinaps
exe lives under a digest-named subdir; locate the `.cmo` first.)

  $ CMO=$(find _build -name 'dune__exe__Cinaps.cmo' | head -1)
  $ dune rules --root . --format=json --deps "$CMO" > deps.json
  $ jq_dune -r '.[] | depsGlobs
  >   | select(.dir | endswith("hello/.hello.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/hello/.hello.objs/byte *.cmi
