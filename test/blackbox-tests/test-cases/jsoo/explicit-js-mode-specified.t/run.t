Check that .bc.js rule is generated only if js mode is used.

  $ dune build a.bc.js
  Error: Don't know how to build a.bc.js
  Hint: did you mean b.bc.js or e.bc.js?
  [1]

  $ dune build b.bc.js

We also check that .cmo.js rules are not generated if not specified.

  $ dune build _build/default/.a.eobjs/jsoo/a.cmo.js
  Error: Don't know how to build _build/default/.a.eobjs/jsoo/a.cmo.js
  [1]

JS compilation of libraries is always available to avoid having to annotate
every dependency of an executable.

  $ dune build _build/default/.foo.objs/jsoo/default/foo.cma.js

Check that js targets are attached to @all, but not for tests that do not
specify js mode (#1940).

  $ dune clean
  $ dune build --display short @@all 2>&1 | grep js_of_ocaml
   js_of_ocaml .b.eobjs/jsoo/b.bc.runtime.js
   js_of_ocaml .e.eobjs/jsoo/e.bc.runtime.js
   js_of_ocaml .js/default/stdlib/std_exit.cmo.js
   js_of_ocaml .js/default/stdlib/stdlib.cma.js
   js_of_ocaml .foo.objs/jsoo/default/foo.cmo.js
   js_of_ocaml .b.eobjs/jsoo/default/b.cmo.js
   js_of_ocaml .foo.objs/jsoo/default/foo__C.cmo.js
   js_of_ocaml b.bc.js
   js_of_ocaml .foo.objs/jsoo/default/foo.cma.js
   js_of_ocaml .e.eobjs/jsoo/default/e.cmo.js
   js_of_ocaml e.bc.js

Check that building a JS-enabled executable that depends on a library works.

  $ dune clean
  $ dune build e.bc.js
