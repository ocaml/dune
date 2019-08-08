Check that .bc.js rule is generated only if js mode is used.

  $ dune build --root mode-specified --display short a.bc.js
  Entering directory 'mode-specified'
  Error: Don't know how to build a.bc.js
  Hint: did you mean b.bc.js or e.bc.js?
  [1]

  $ dune build --root mode-specified --display short b.bc.js
  Entering directory 'mode-specified'
   js_of_ocaml b.bc.runtime.js
      ocamldep .b.eobjs/b.ml.d
        ocamlc .b.eobjs/byte/b.{cmi,cmo,cmt}
   js_of_ocaml .b.eobjs/byte/b.cmo.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
     jsoo_link b.bc.js

We also check that .cmo.js rules are not generated if not specified.

  $ dune build --root mode-specified --display short _build/default/.a.eobjs/byte/a.cmo.js
  Entering directory 'mode-specified'
  Error: Don't know how to build _build/default/.a.eobjs/byte/a.cmo.js
  [1]

JS compilation of libraries is always available to avoid having to annotate
every dependency of an executable.

  $ dune build --root mode-specified --display short _build/default/.foo.objs/foo.cma.js
  Entering directory 'mode-specified'
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamldep .foo.objs/c.ml.d
        ocamlc .foo.objs/byte/foo__C.{cmi,cmo,cmt}
        ocamlc foo.cma
   js_of_ocaml .foo.objs/foo.cma.js

Check that js targets are attached to @all, but not for tests that do not
specify js mode (#1940).

  $ dune clean
  $ dune build --root mode-specified --display short @@all 2>&1 | grep js_of_ocaml
   js_of_ocaml e.bc.runtime.js
   js_of_ocaml .e.eobjs/byte/e.cmo.js

Check that building a JS-enabled executable that depends on a library works.

  $ dune clean
  $ dune build --root mode-specified --display short e.bc.js
  Entering directory 'mode-specified'

Anonymous projects have explicit_js_mode enabled

  $ dune build --root anon --display short @all
  Entering directory 'anon'
  Info: Creating file dune-project with this contents:
  | (lang dune 2.0)
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt .foo.objs/native/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
