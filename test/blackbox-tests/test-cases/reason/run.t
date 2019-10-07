  $ dune build @runtest @install-file
  lib: [
    "_build/install/default/lib/rlib/META"
    "_build/install/default/lib/rlib/bar.mli"
    "_build/install/default/lib/rlib/bar.re"
    "_build/install/default/lib/rlib/cppome.re"
    "_build/install/default/lib/rlib/cppome.rei"
    "_build/install/default/lib/rlib/dune-package"
    "_build/install/default/lib/rlib/foo.ml"
    "_build/install/default/lib/rlib/foo.rei"
    "_build/install/default/lib/rlib/hello.re"
    "_build/install/default/lib/rlib/hello.rei"
    "_build/install/default/lib/rlib/opam"
    "_build/install/default/lib/rlib/pped.re"
    "_build/install/default/lib/rlib/pped.rei"
    "_build/install/default/lib/rlib/rlib$ext_lib"
    "_build/install/default/lib/rlib/rlib.cma"
    "_build/install/default/lib/rlib/rlib.cmi"
    "_build/install/default/lib/rlib/rlib.cmt"
    "_build/install/default/lib/rlib/rlib.cmx"
    "_build/install/default/lib/rlib/rlib.cmxa"
    "_build/install/default/lib/rlib/rlib.cmxs"
    "_build/install/default/lib/rlib/rlib.ml"
    "_build/install/default/lib/rlib/rlib__Bar.cmi"
    "_build/install/default/lib/rlib/rlib__Bar.cmt"
    "_build/install/default/lib/rlib/rlib__Bar.cmti"
    "_build/install/default/lib/rlib/rlib__Bar.cmx"
    "_build/install/default/lib/rlib/rlib__Cppome.cmi"
    "_build/install/default/lib/rlib/rlib__Cppome.cmt"
    "_build/install/default/lib/rlib/rlib__Cppome.cmti"
    "_build/install/default/lib/rlib/rlib__Cppome.cmx"
    "_build/install/default/lib/rlib/rlib__Foo.cmi"
    "_build/install/default/lib/rlib/rlib__Foo.cmt"
    "_build/install/default/lib/rlib/rlib__Foo.cmti"
    "_build/install/default/lib/rlib/rlib__Foo.cmx"
    "_build/install/default/lib/rlib/rlib__Hello.cmi"
    "_build/install/default/lib/rlib/rlib__Hello.cmt"
    "_build/install/default/lib/rlib/rlib__Hello.cmti"
    "_build/install/default/lib/rlib/rlib__Hello.cmx"
    "_build/install/default/lib/rlib/rlib__Pped.cmi"
    "_build/install/default/lib/rlib/rlib__Pped.cmt"
    "_build/install/default/lib/rlib/rlib__Pped.cmti"
    "_build/install/default/lib/rlib/rlib__Pped.cmx"
  ]
  bin: [
    "_build/install/default/bin/refmt"
  ]
          rbin alias runtest
  Cppome
  hello world
  Bar
  Foo

virtual libraries in reason
  $ PATH="_build/install/default/bin:$PATH" dune build --root vlib-impl @all
  Entering directory 'vlib-impl'
