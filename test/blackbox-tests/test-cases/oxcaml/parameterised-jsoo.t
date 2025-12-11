Testing that js_of_ocaml works with the instantiation of parameterised libs.
At the moment, js_of_ocaml does not support the separate compilation of
parameterised instances, so only whole program compilation is available.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using oxcaml 0.1)
  > EOF

First define a parameter:

  $ mkdir param
  $ echo 'val param : string' > param/param.mli
  $ cat > param/dune <<EOF
  > (library_parameter (name param))
  > EOF

Then an implementation of this parameter:

  $ mkdir impl
  $ echo 'let param = Util.util' > impl/impl.ml
  $ echo 'let util = "impl"' > impl/util.ml
  $ cat > impl/dune <<EOF
  > (library (name impl) (implements param))
  > EOF

And another implementation:

  $ mkdir impl2
  $ echo 'let param = "impl2"' > impl2/impl2.ml
  $ cat > impl2/dune <<EOF
  > (library (name impl2) (implements param))
  > EOF

Then a parameterised library:

  $ mkdir lib
  $ echo 'let lib () = "lib(" ^ Param.param ^ ") " ^ Helper.helper' > lib/lib.ml
  $ echo 'let helper = "helper(" ^ Param.param ^ ")"' > lib/helper.ml
  $ cat > lib/dune <<EOF
  > (library (name lib) (parameters param))
  > EOF

And another parameterised library:

  $ mkdir lib2
  $ echo 'let lib2 () = "lib2(" ^ Lib_param.lib () ^ ", " ^ Lib_impl2.lib () ^ ")"' > lib2/lib2.ml
  $ cat > lib2/dune <<EOF
  > (library
  >   (name lib2)
  >   (parameters param)
  >   (libraries
  >     (instantiate lib :as lib_param)
  >     (instantiate lib impl2 :as lib_impl2)))
  > EOF

Then an executable, with a couple more instantiations of parameterised libraries:

  $ mkdir bin
  $ echo 'let () = A.a (); B.b (); C.c ()' > bin/bin.ml
  $ echo 'let a () = print_endline (Lib2_impl.lib2 ())' > bin/a.ml
  $ echo 'let b () = print_endline (Lib2_impl2.lib2 ())' > bin/b.ml
  $ echo 'let c () = print_endline (Lib_impl.lib ())' > bin/c.ml
  $ cat > bin/dune <<EOF
  > (executable
  >   (name bin)
  >   (modes byte js wasm)
  >   (libraries
  >     (instantiate lib2 impl :as lib2_impl)
  >     (instantiate lib2 impl2 :as lib2_impl2)
  >     (instantiate lib impl :as lib_impl)))
  > EOF

  $ dune exec ./bin/bin.exe
  lib2(lib(impl) helper(impl), lib(impl2) helper(impl2))
  lib2(lib(impl2) helper(impl2), lib(impl2) helper(impl2))
  lib(impl) helper(impl)

Testing byte compilation:

  $ dune exec ./bin/bin.bc
  lib2(lib(impl) helper(impl), lib(impl2) helper(impl2))
  lib2(lib(impl2) helper(impl2), lib(impl2) helper(impl2))
  lib(impl) helper(impl)

Testing js_of_ocaml:

  $ dune build ./bin/bin.bc.js
  $ node _build/default/bin/bin.bc.js
  lib2(lib(impl) helper(impl), lib(impl2) helper(impl2))
  lib2(lib(impl2) helper(impl2), lib(impl2) helper(impl2))
  lib(impl) helper(impl)

Testing wasm_of_ocaml:

  $ dune build ./bin/bin.bc.wasm.js
  $ node _build/default/bin/bin.bc.wasm.js
  lib2(lib(impl) helper(impl), lib(impl2) helper(impl2))
  lib2(lib(impl2) helper(impl2), lib(impl2) helper(impl2))
  lib(impl) helper(impl)
