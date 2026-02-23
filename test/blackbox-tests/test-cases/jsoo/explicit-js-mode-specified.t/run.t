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
  $ dune build @@all
  $ dune trace cat | jq -r 'include "dune";
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' | sort
  .b.eobjs/jsoo/b.cmo.js
  .e.eobjs/jsoo/e.cmo.js
  .foo.objs/jsoo/default/foo.cma.js
  .js/default/.runtime/69326c30fc4a6ffc800b0a8e0b822993/runtime.bc.runtime.js
  .js/default/stdlib/std_exit.cmo.js
  .js/default/stdlib/stdlib.cma.js
  b.bc.js
  e.bc.js

Check that building a JS-enabled executable that depends on a library works.

  $ dune clean
  $ dune build e.bc.js
