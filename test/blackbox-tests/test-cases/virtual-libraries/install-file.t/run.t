Install files for implementations and virtual libs have all the artifacts:
  $ dune build 2>&1 | dune_cmd sanitize
  lib: [
    "_build/install/default/lib/vlib/META"
    "_build/install/default/lib/vlib/dune-package"
    "_build/install/default/lib/vlib/foo.mli"
    "_build/install/default/lib/vlib/opam"
    "_build/install/default/lib/vlib/vlib.cmi"
    "_build/install/default/lib/vlib/vlib.cmo"
    "_build/install/default/lib/vlib/vlib.cmt"
    "_build/install/default/lib/vlib/vlib.cmx"
    "_build/install/default/lib/vlib/vlib.ml"
    "_build/install/default/lib/vlib/vlib$ext_obj"
    "_build/install/default/lib/vlib/vlib__Foo.cmi"
    "_build/install/default/lib/vlib/vlib__Foo.cmti"
  ]
  lib: [
    "_build/install/default/lib/impl/META"
    "_build/install/default/lib/impl/dune-package"
    "_build/install/default/lib/impl/foo.ml"
    "_build/install/default/lib/impl/impl$ext_lib"
    "_build/install/default/lib/impl/impl.cma"
    "_build/install/default/lib/impl/impl.cmxa"
    "_build/install/default/lib/impl/opam"
    "_build/install/default/lib/impl/vlib.cmi"
    "_build/install/default/lib/impl/vlib.cmx"
    "_build/install/default/lib/impl/vlib__Foo.cmi"
    "_build/install/default/lib/impl/vlib__Foo.cmt"
    "_build/install/default/lib/impl/vlib__Foo.cmx"
    "_build/install/default/lib/impl/vlib__impl__.cmi"
    "_build/install/default/lib/impl/vlib__impl__.cmt"
    "_build/install/default/lib/impl/vlib__impl__.cmx"
    "_build/install/default/lib/impl/vlib__impl__.ml"
  ]
  libexec: [
    "_build/install/default/lib/impl/impl.cmxs"
  ]
