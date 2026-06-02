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

`hello` is the ppx runtime lib and `hello_ppx` is a no-op
ppx_rewriter declaring it as a runtime lib. The helper uses the
stub-driver pattern from `ppx-runtime-libraries.t`.

  $ make_hello_ppx_runtime_fixture melange

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
