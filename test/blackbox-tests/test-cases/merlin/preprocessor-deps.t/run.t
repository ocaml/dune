Dune advertises the files listed in `(preprocessor_deps ...)` to Merlin using
`PPX_DEPS` directives, so that Merlin can invalidate its PPX cache when one of
them changes.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

A trivial rewriter, and two libraries using it: one that declares preprocessor
dependencies, and one that does not.

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name my_ppx)
  >  (kind ppx_rewriter))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name with_deps)
  >  (modules with_deps)
  >  (preprocess (pps my_ppx))
  >  (preprocessor_deps (file ppx_dep.txt) (glob_files *.data)))
  > 
  > (library
  >  (name without_deps)
  >  (modules without_deps)
  >  (preprocess (pps my_ppx)))
  > EOF

  $ touch ppx_dep.txt static.data other.data

  $ cat > with_deps.ml <<EOF
  > let x = 1
  > EOF
  $ cat > without_deps.ml <<EOF
  > let y = 2
  > EOF


The dependencies are advertised as absolute paths, with globs expanded.

  $ dune build .merlin-conf/lib-with_deps
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   merlinEntry("With_deps")
  >   | merlinJsonEntryWithConfigNames(["PPX_DEPS"])'
  With_deps: _build/default/with_deps
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/other.data"]
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/ppx_dep.txt"]
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/static.data"]
  With_deps: _build/default/with_deps.ml
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/other.data"]
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/ppx_dep.txt"]
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/static.data"]

The library that declares none gets no `PPX_DEPS` directive.

  $ dune build .merlin-conf/lib-without_deps
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   merlinEntry("Without_deps")
  >   | merlinJsonEntryWithConfigNames(["PPX_DEPS"])
  > '
  Without_deps: _build/default/without_deps
  Without_deps: _build/default/without_deps.ml

Files referenced with `%{dep:...}` in the PPX arguments are also dependencies of
the preprocessor, and are advertised as well.

  $ cat >> dune <<EOF
  > (library
  >  (name with_flag_deps)
  >  (modules with_flag_deps)
  >  (preprocess (pps my_ppx -- --input %{dep:input.txt})))
  > EOF

  $ touch input.txt
  $ cat > with_flag_deps.ml <<EOF
  > let z = 3
  > EOF

  $ dune build .merlin-conf/lib-with_flag_deps
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   merlinEntry("With_flag_deps")
  >   | merlinJsonEntryWithConfigNames(["PPX_DEPS"])
  > '
  With_flag_deps: _build/default/with_flag_deps
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/input.txt"]
  With_flag_deps: _build/default/with_flag_deps.ml
  ["PPX_DEPS","$TESTCASE_ROOT/_build/default/input.txt"]

