This test checks that ppx rewriters can declare different runtime libraries for
native and Melange compilation.

The field is available starting in Dune 3.24.

  $ mkdir old-lang
  $ cat > old-lang/dune-project <<EOF
  > (lang dune 3.23)
  > (using melange 0.1)
  > EOF
  $ cat > old-lang/dune <<EOF
  > (library
  >  (name ppx)
  >  (kind ppx_rewriter)
  >  (melange.ppx_runtime_libraries runtime))
  > EOF
  $ dune build --root old-lang
  Entering directory 'old-lang'
  File "dune", line 4, characters 1-40:
  4 |  (melange.ppx_runtime_libraries runtime))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.ppx_runtime_libraries' is only available since version 3.24
  of the dune language. Please update your dune-project file to have (lang dune
  3.24).
  Leaving directory 'old-lang'
  [1]
  $ rm -r old-lang

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (package (name my-ppx))
  > (package (name foo))
  > (package (name mel-foo) (allow_empty))
  > (using melange 0.1)
  > EOF

Define a runtime library that the ppx will depend on via `(ppx_runtime_libraries)`

  $ mkdir runtime
  $ cat > runtime/dune <<EOF
  > (library
  >  (name runtime)
  >  (public_name my-ppx.runtime)
  >  (modules runtime)
  >  (modes melange))
  > (library
  >  (name native)
  >  (public_name my-ppx.native-runtime)
  >  (modules native))
  > EOF
  $ cat > runtime/runtime.ml <<EOF
  > let msg = "melange runtime"
  > EOF
  $ cat > runtime/native.ml <<EOF
  > let msg = "native runtime"
  > EOF

Define a PPX rewriter that has different runtime libs (native / melange)

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name my_ppx)
  >  (public_name my-ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries my-ppx.native-runtime)
  >  (melange.ppx_runtime_libraries my-ppx.runtime))
  > EOF
  $ cat > ppx/my_ppx.ml <<EOF
  > open Ppxlib
  > let () = Driver.register_transformation "my_ppx"
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (melange.emit
  >  (package mel-foo)
  >  (target js-out)
  >  (preprocess (pps my-ppx))
  >  (emit_stdlib false))
  > EOF
  $ cat > src/app.ml <<EOF
  > let () = Js.log Runtime.msg
  > EOF

  $ js_out=_build/default/src/js-out
  $ dune build \
  >   "$js_out/node_modules/my-ppx.runtime/runtime.js" \
  >   "$js_out/src/app.js"
  $ find "$js_out" -type f | sort
  _build/default/src/js-out/node_modules/my-ppx.runtime/runtime.js
  _build/default/src/js-out/src/app.js
  $ node "$js_out/src/app.js"
  melange runtime

  $ mkdir bin
  $ cat > bin/dune <<EOF
  > (executable
  >  (public_name app)
  >  (package foo)
  >  (preprocess (pps my-ppx)))
  > EOF
  $ cat > bin/app.ml <<EOF
  > let () = Format.eprintf "%s" Native.msg
  > EOF

  $ dune exec bin/app.exe
  native runtime

The same ppx runtime dependency information is preserved after installation.
This exercises decoding [melange_ppx_runtime_deps] from the installed
[dune-package] file of a native-only ppx rewriter.

  $ dune build @install
  $ dune install --prefix $PWD/prefix > /dev/null

  $ mkdir installed-consumer
  $ cat > installed-consumer/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name installed-consumer))
  > (using melange 0.1)
  > EOF
  $ cat > installed-consumer/dune <<EOF
  > (melange.emit
  >  (package installed-consumer)
  >  (target js-out)
  >  (preprocess (pps my-ppx))
  >  (emit_stdlib false))
  > EOF
  $ cat > installed-consumer/app.ml <<EOF
  > let () = Js.log Runtime.msg
  > EOF

  $ consumer_js_out=_build/default/js-out
  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root installed-consumer \
  >   "$consumer_js_out/app.js" \
  >   "$consumer_js_out/node_modules/my-ppx.runtime/runtime.js" \
  >   --display=quiet
  $ find "installed-consumer/$consumer_js_out" -type f | sort
  installed-consumer/_build/default/js-out/app.js
  installed-consumer/_build/default/js-out/node_modules/my-ppx.runtime/runtime.js
  $ node "installed-consumer/$consumer_js_out/app.js"
  melange runtime
