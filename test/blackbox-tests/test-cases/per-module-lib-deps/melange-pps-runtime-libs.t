A `(melange.emit ...)` stanza with `(preprocess (pps hello_ppx))`
— where `hello_ppx` declares `(ppx_runtime_libraries hello)` —
must pull `hello` into the melange compile-rule dep set. Pins
that the compile rule has globs over `hello`'s melange objdir,
sourced from the ppx runtime declaration alone — never named in
source.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF

`hello` is the ppx runtime lib (built for melange):

  $ mkdir hello
  $ cat > hello/dune <<EOF
  > (library (name hello) (modes melange))
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
  >     ; ("-loc-filename", Arg.String ignore, "")
  >     ]
  >   in
  >   let anon _ = () in
  >   Arg.parse (Arg.align args) anon "";
  >   close_out (open_out !out)
  > EOF

The melange.emit preprocesses with `hello_ppx`:

  $ mkdir me
  $ cat > me/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (preprocess (pps hello_ppx)))
  > EOF
  $ cat > me/foo.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build

The melange compile rule has globs over `hello`'s melange objdir
— sourced from `hello_ppx`'s `ppx_runtime_libraries`, with no
syntactic reference to `Hello` in melange sources. (The melange
target's modules are mangled internally, so `%{cmj:...}` doesn't
resolve cleanly; locate the `.cmj` under the stanza dir.)

  $ CMJ=$(find _build/default/me -name '*.cmj' | head -1)
  $ dune rules --root . --format=json --deps "$CMJ" > deps.json
  $ jq_dune -r '.[] | depsGlobs
  >   | select(.dir | endswith("hello/.hello.objs/melange"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/hello/.hello.objs/melange *.cmi
  _build/default/hello/.hello.objs/melange *.cmj
