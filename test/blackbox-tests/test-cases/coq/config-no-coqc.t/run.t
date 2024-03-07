Testing for https://github.com/ocaml/dune/issues/9818

We first test the package builds as normal, when both are in scope:

  $ dune build -p example-ocaml
  $ dune build -p example-coq
  $ cat example-ocaml.install
  lib: [
    "_build/install/default/lib/example-ocaml/META"
    "_build/install/default/lib/example-ocaml/bar/bar.a" {"bar/bar.a"}
    "_build/install/default/lib/example-ocaml/bar/bar.cma" {"bar/bar.cma"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmi" {"bar/bar.cmi"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmt" {"bar/bar.cmt"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmx" {"bar/bar.cmx"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmxa" {"bar/bar.cmxa"}
    "_build/install/default/lib/example-ocaml/bar/bar.ml" {"bar/bar.ml"}
    "_build/install/default/lib/example-ocaml/dune-package"
    "_build/install/default/lib/example-ocaml/opam"
  ]
  libexec: [
    "_build/install/default/lib/example-ocaml/bar/bar.cmxs" {"bar/bar.cmxs"}
  ]
  doc: [
    "_build/install/default/doc/example-ocaml/README.md"
  ]
  $ cat example-coq.install
  lib: [
    "_build/install/default/lib/example-coq/META"
    "_build/install/default/lib/example-coq/coq/CRelationClasses.ml" {"coq/CRelationClasses.ml"}
    "_build/install/default/lib/example-coq/coq/CRelationClasses.mli" {"coq/CRelationClasses.mli"}
    "_build/install/default/lib/example-coq/coq/Datatypes.ml" {"coq/Datatypes.ml"}
    "_build/install/default/lib/example-coq/coq/Datatypes.mli" {"coq/Datatypes.mli"}
    "_build/install/default/lib/example-coq/coq/Foo.ml" {"coq/Foo.ml"}
    "_build/install/default/lib/example-coq/coq/Foo.mli" {"coq/Foo.mli"}
    "_build/install/default/lib/example-coq/coq/cRelationClasses.cmi" {"coq/cRelationClasses.cmi"}
    "_build/install/default/lib/example-coq/coq/cRelationClasses.cmt" {"coq/cRelationClasses.cmt"}
    "_build/install/default/lib/example-coq/coq/cRelationClasses.cmti" {"coq/cRelationClasses.cmti"}
    "_build/install/default/lib/example-coq/coq/cRelationClasses.cmx" {"coq/cRelationClasses.cmx"}
    "_build/install/default/lib/example-coq/coq/datatypes.cmi" {"coq/datatypes.cmi"}
    "_build/install/default/lib/example-coq/coq/datatypes.cmt" {"coq/datatypes.cmt"}
    "_build/install/default/lib/example-coq/coq/datatypes.cmti" {"coq/datatypes.cmti"}
    "_build/install/default/lib/example-coq/coq/datatypes.cmx" {"coq/datatypes.cmx"}
    "_build/install/default/lib/example-coq/coq/extracted.a" {"coq/extracted.a"}
    "_build/install/default/lib/example-coq/coq/extracted.cma" {"coq/extracted.cma"}
    "_build/install/default/lib/example-coq/coq/extracted.cmxa" {"coq/extracted.cmxa"}
    "_build/install/default/lib/example-coq/coq/foo.cmi" {"coq/foo.cmi"}
    "_build/install/default/lib/example-coq/coq/foo.cmt" {"coq/foo.cmt"}
    "_build/install/default/lib/example-coq/coq/foo.cmti" {"coq/foo.cmti"}
    "_build/install/default/lib/example-coq/coq/foo.cmx" {"coq/foo.cmx"}
    "_build/install/default/lib/example-coq/dune-package"
    "_build/install/default/lib/example-coq/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/Common/Foo.v" {"coq/user-contrib/Common/Foo.v"}
    "_build/install/default/lib/coq/user-contrib/Common/Foo.vo" {"coq/user-contrib/Common/Foo.vo"}
  ]
  libexec: [
    "_build/install/default/lib/example-coq/coq/extracted.cmxs" {"coq/extracted.cmxs"}
  ]
  doc: [
    "_build/install/default/doc/example-coq/README.md"
  ]
  $ cp example-ocaml.install example-ocaml.install.old
  $ dune clean

We now test that the ocaml package still builds even when Coq rules can't be setup:

  $ mkdir _path
  $ ln -s $(command -v dune) _path/
  $ ln -s $(command -v ocamlc) _path/
  $ ln -s $(command -v ocamldep) _path/

  $ (unset INSIDE_DUNE; PATH=_path dune build -p example-ocaml)
  $ cat example-ocaml.install
  lib: [
    "_build/install/default/lib/example-ocaml/META"
    "_build/install/default/lib/example-ocaml/bar/bar.cma" {"bar/bar.cma"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmi" {"bar/bar.cmi"}
    "_build/install/default/lib/example-ocaml/bar/bar.cmt" {"bar/bar.cmt"}
    "_build/install/default/lib/example-ocaml/bar/bar.ml" {"bar/bar.ml"}
    "_build/install/default/lib/example-ocaml/dune-package"
    "_build/install/default/lib/example-ocaml/opam"
  ]
  doc: [
    "_build/install/default/doc/example-ocaml/README.md"
  ]
  $ diff -u --label install_old example-ocaml.install.old --label install_new example-ocaml.install
  --- install_old
  +++ install_new
  @@ -1,18 +1,12 @@
   lib: [
     "_build/install/default/lib/example-ocaml/META"
  -  "_build/install/default/lib/example-ocaml/bar/bar.a" {"bar/bar.a"}
     "_build/install/default/lib/example-ocaml/bar/bar.cma" {"bar/bar.cma"}
     "_build/install/default/lib/example-ocaml/bar/bar.cmi" {"bar/bar.cmi"}
     "_build/install/default/lib/example-ocaml/bar/bar.cmt" {"bar/bar.cmt"}
  -  "_build/install/default/lib/example-ocaml/bar/bar.cmx" {"bar/bar.cmx"}
  -  "_build/install/default/lib/example-ocaml/bar/bar.cmxa" {"bar/bar.cmxa"}
     "_build/install/default/lib/example-ocaml/bar/bar.ml" {"bar/bar.ml"}
     "_build/install/default/lib/example-ocaml/dune-package"
     "_build/install/default/lib/example-ocaml/opam"
   ]
  -libexec: [
  -  "_build/install/default/lib/example-ocaml/bar/bar.cmxs" {"bar/bar.cmxs"}
  -]
   doc: [
     "_build/install/default/doc/example-ocaml/README.md"
   ]
  [1]

Coq package should fail:

  $ (unset INSIDE_DUNE; PATH=_path dune build -p example-coq)
  Couldn't find Coq standard library, and theory is not using (stdlib no)
  -> required by _build/default/coq/extracted/CRelationClasses.ml
  -> required by _build/default/coq/CRelationClasses.ml
  -> required by _build/install/default/lib/example-coq/coq/CRelationClasses.ml
  -> required by _build/default/example-coq.install
  -> required by alias install
  [1]
  $ cat example-coq.install
  cat: example-coq.install: No such file or directory
  [1]
