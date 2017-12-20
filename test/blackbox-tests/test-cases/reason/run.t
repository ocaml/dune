  $ $JBUILDER build @runtest @install-file -j1 --root .
         refmt bar.re.ml
      ocamldep pp/reasononlypp.depends.ocamldep-output
      ocamldep ppx/reasonppx.depends.ocamldep-output
         refmt hello.re.ml
         refmt pped.re.ml
        ocamlc rlib.{cmi,cmo,cmt}
         refmt foo.re.mli
         refmt hello.re.mli
         refmt pped.re.mli
        ocamlc pp/reasononlypp.{cmi,cmo,cmt}
        ocamlc ppx/reasonppx.{cmi,cmo,cmt}
      ocamlopt rlib.{cmx,o}
      ocamlopt pp/reasononlypp.{cmx,o}
      ocamlopt ppx/reasonppx.{cmx,o}
      ocamlopt pp/reasononlypp.exe
      ocamlopt ppx/reasonppx.{a,cmxa}
  reasononlypp cppome.pp.re
  reasononlypp cppome.pp.rei
  reasononlypp rbin.pp.re
      ocamlopt .ppx/reasonppx/ppx.exe
         refmt cppome.pp.re.ml
         refmt cppome.pp.re.mli
         refmt rbin.pp.re.ml
           ppx foo.pp.ml
           ppx hello.re.pp.ml
           ppx foo.re.pp.mli
           ppx hello.re.pp.mli
      ocamldep rbin.depends.ocamldep-output
      ocamldep rlib.depends.ocamldep-output
      ocamldep rlib.dependsi.ocamldep-output
        ocamlc rlib__Bar.{cmi,cmti}
        ocamlc rlib__Cppome.{cmi,cmti}
        ocamlc rlib__Foo.{cmi,cmti}
        ocamlc rlib__Hello.{cmi,cmti}
        ocamlc rlib__Pped.{cmi,cmti}
        ocamlc rlib__Bar.{cmo,cmt}
      ocamlopt rlib__Bar.{cmx,o}
        ocamlc rlib__Cppome.{cmo,cmt}
      ocamlopt rlib__Cppome.{cmx,o}
        ocamlc rlib__Foo.{cmo,cmt}
      ocamlopt rlib__Foo.{cmx,o}
        ocamlc rlib__Hello.{cmo,cmt}
      ocamlopt rlib__Hello.{cmx,o}
        ocamlc rlib__Pped.{cmo,cmt}
      ocamlopt rlib__Pped.{cmx,o}
        ocamlc rbin.{cmi,cmo,cmt}
        ocamlc rlib.cma
      ocamlopt rlib.{a,cmxa}
      ocamlopt rbin.{cmx,o}
      ocamlopt rlib.cmxs
  lib: [
    "_build/install/default/lib/rlib/META" {"META"}
    "_build/install/default/lib/rlib/opam" {"opam"}
    "_build/install/default/lib/rlib/rlib__Bar.cmi"
    "_build/install/default/lib/rlib/rlib__Bar.cmx"
    "_build/install/default/lib/rlib/rlib__Bar.cmt"
    "_build/install/default/lib/rlib/rlib__Bar.cmti"
    "_build/install/default/lib/rlib/bar.mli"
    "_build/install/default/lib/rlib/rlib__Cppome.cmi"
    "_build/install/default/lib/rlib/rlib__Cppome.cmx"
    "_build/install/default/lib/rlib/rlib__Cppome.cmt"
    "_build/install/default/lib/rlib/rlib__Cppome.cmti"
    "_build/install/default/lib/rlib/cppome.rei"
    "_build/install/default/lib/rlib/rlib__Foo.cmi"
    "_build/install/default/lib/rlib/rlib__Foo.cmx"
    "_build/install/default/lib/rlib/rlib__Foo.cmt"
    "_build/install/default/lib/rlib/rlib__Foo.cmti"
    "_build/install/default/lib/rlib/foo.rei"
    "_build/install/default/lib/rlib/rlib__Hello.cmi"
    "_build/install/default/lib/rlib/rlib__Hello.cmx"
    "_build/install/default/lib/rlib/rlib__Hello.cmt"
    "_build/install/default/lib/rlib/rlib__Hello.cmti"
    "_build/install/default/lib/rlib/hello.rei"
    "_build/install/default/lib/rlib/rlib__Pped.cmi"
    "_build/install/default/lib/rlib/rlib__Pped.cmx"
    "_build/install/default/lib/rlib/rlib__Pped.cmt"
    "_build/install/default/lib/rlib/rlib__Pped.cmti"
    "_build/install/default/lib/rlib/pped.rei"
    "_build/install/default/lib/rlib/rlib.cmi"
    "_build/install/default/lib/rlib/rlib.cmx"
    "_build/install/default/lib/rlib/rlib.cmt"
    "_build/install/default/lib/rlib/rlib.ml-gen"
    "_build/install/default/lib/rlib/rlib.cma"
    "_build/install/default/lib/rlib/rlib.cmxa"
    "_build/install/default/lib/rlib/rlib.a"
    "_build/install/default/lib/rlib/rlib.cmxs"
  ]
