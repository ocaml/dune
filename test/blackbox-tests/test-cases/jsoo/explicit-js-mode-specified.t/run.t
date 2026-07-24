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
every dependency of an executable. The library's cma.js target shows up in
the @all trace below.

Check that js targets are attached to @all, but not for tests that do not
specify js mode (#1940).

  $ dune clean
  $ dune build @@all
Sort by shape (hex normalized to a placeholder) so digest labels get
assigned deterministically regardless of raw digest byte values.

  $ dune trace cat | jq_dune -r '
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")' \
  > | awk '{ k=$0; gsub(/[0-9a-f]{32}/, "HEX", k); print k "\t" $0 }' \
  > | sort \
  > | cut -f2- \
  > | censor
  .b.eobjs/jsoo/b.cmo.js
  .e.eobjs/jsoo/e.cmo.js
  .foo.objs/jsoo/$DIGEST1/foo.cma.js
  .js/$DIGEST2/runtime.bc.runtime.js
  .js/$DIGEST1/stdlib/std_exit.cmo.js
  .js/$DIGEST1/stdlib/stdlib.cma.js
  b.bc.js
  e.bc.js

Check that building a JS-enabled executable that depends on a library works.

  $ dune clean
  $ dune build e.bc.js
