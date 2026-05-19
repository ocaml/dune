A `(cinaps ...)` stanza with `(preprocess (pps hello_ppx))` —
where `hello_ppx` declares `(ppx_runtime_libraries hello)` — must
pull `hello` into the cinaps exe's compile-rule dep set. Pins
that the compile rule has a glob over `hello`'s byte objdir,
sourced from the ppx runtime declaration alone — never named in
source.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using cinaps 1.0)
  > EOF

`hello` is the ppx runtime lib:

  $ mkdir hello
  $ cat > hello/dune <<EOF
  > (library (name hello))
  > EOF
  $ cat > hello/hello.ml <<EOF
  > type t = int
  > EOF

`hello_ppx` is a no-op ppx_rewriter declaring `hello` as a
runtime lib. The `(ppx.driver ...)` form satisfies dune's
pps-invocation contract without pulling in ppxlib (matches the
stub-driver pattern in `ppx-runtime-libraries.t`).

  $ mkdir hello_ppx
  $ cat > hello_ppx/dune <<EOF
  > (library
  >  (name hello_ppx)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries hello)
  >  (ppx.driver (main Hello_ppx.main)))
  > EOF
  $ cat > hello_ppx/hello_ppx.ml <<EOF
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
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("hello/.hello.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/hello/.hello.objs/byte *.cmi
