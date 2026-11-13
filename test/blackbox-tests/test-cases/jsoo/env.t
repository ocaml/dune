Tests env stanzas in JSOO contexts.

  $ make_dune_project 3.0
  $ cat >dune <<EOF
  > (env (_ (js_of_ocaml (flags :standard "--no-inline"))))
  > (library (name test))
  > EOF
  $ dune printenv --field js_of_ocaml_flags --field js_of_ocaml_link_flags --field js_of_ocaml_build_runtime_flags 2>&1
  (js_of_ocaml_flags
   (--pretty --no-inline))
  (js_of_ocaml_build_runtime_flags (--pretty))
  (js_of_ocaml_link_flags ())

Missing link and runtime flag inputs must not affect module compilation.
A fake compiler keeps this check independent of optional backend tools.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (wrapped_executables false)
  > EOF
  $ mkdir bin
  $ cat > bin/js_of_ocaml <<'EOF'
  > #!/bin/sh
  > while [ "$#" -gt 0 ]; do
  >   case "$1" in
  >     --version) echo 5.0.0; exit 0 ;;
  >     -o) printf 'fake backend output\n' >"$2"; exit 0 ;;
  >   esac
  >   shift
  > done
  > exit 1
  > EOF
  $ chmod +x bin/js_of_ocaml
  $ cp bin/js_of_ocaml bin/wasm_of_ocaml
  $ cat > dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes js wasm)
  >  (js_of_ocaml
  >   (link_flags (:include js-link-flags.sexp))
  >   (build_runtime_flags (:include js-runtime-flags.sexp)))
  >  (wasm_of_ocaml
  >   (link_flags (:include wasm-link-flags.sexp))
  >   (build_runtime_flags (:include wasm-runtime-flags.sexp))))
  > EOF
  $ echo 'let value = 42' > a.ml
  $ PATH="$PWD/bin:$PATH" dune build .a.eobjs/jsoo/a.cmo.js .a.eobjs/jsoo/a.wasmo
  $ cat _build/default/.a.eobjs/jsoo/a.cmo.js _build/default/.a.eobjs/jsoo/a.wasmo
  fake backend output
  fake backend output

Selected compilation flags must still report missing inputs for both backends.

  $ cat > dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes js wasm)
  >  (js_of_ocaml (flags (:include js-compile-flags.sexp)))
  >  (wasm_of_ocaml (flags (:include wasm-compile-flags.sexp))))
  > EOF
  $ PATH="$PWD/bin:$PATH" dune build .a.eobjs/jsoo/a.cmo.js > js-flags.log 2>&1
  [1]
  $ grep -q 'No rule found for js-compile-flags.sexp' js-flags.log
  $ PATH="$PWD/bin:$PATH" dune build .a.eobjs/jsoo/a.wasmo > wasm-flags.log 2>&1
  [1]
  $ grep -q 'No rule found for wasm-compile-flags.sexp' wasm-flags.log
