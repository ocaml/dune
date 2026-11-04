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

Some dependencies cannot be expressed as a list of files. In that case, Merlin's
PPX cache cannot be invalidated correctly, so it should be disabled:
`USE_PPX_CACHE` should not be emitted.

  $ cat >> dune <<EOF
  > (rule
  >  (alias some_alias)
  >  (action (progn)))
  > 
  > (library
  >  (name with_alias_dep)
  >  (modules with_alias_dep)
  >  (preprocess (pps my_ppx))
  >  (preprocessor_deps (alias some_alias)))
  > 
  > (library
  >  (name with_universe_dep)
  >  (modules with_universe_dep)
  >  (preprocess (pps my_ppx))
  >  (preprocessor_deps (universe)))
  > 
  > (library
  >  (name with_env_dep)
  >  (modules with_env_dep)
  >  (preprocess (pps my_ppx))
  >  (preprocessor_deps (env_var SOME_VAR)))
  > EOF

  $ for m in with_alias_dep with_universe_dep with_env_dep; do
  >   echo "let x = 1" > $m.ml
  > done

  $ dune build .merlin-conf/lib-with_alias_dep \
  >   .merlin-conf/lib-with_universe_dep .merlin-conf/lib-with_env_dep
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   .[] | select(.module_name
  >     | IN("With_alias_dep", "With_universe_dep", "With_env_dep"))
  >   | merlinJsonEntryWithConfigNames(["PPX_DEPS", "USE_PPX_CACHE"])
  > '
  With_alias_dep: _build/default/with_alias_dep
  ["USE_PPX_CACHE"]
  With_alias_dep: _build/default/with_alias_dep.ml
  ["USE_PPX_CACHE"]
  With_env_dep: _build/default/with_env_dep
  ["USE_PPX_CACHE"]
  With_env_dep: _build/default/with_env_dep.ml
  ["USE_PPX_CACHE"]
  With_universe_dep: _build/default/with_universe_dep
  ["USE_PPX_CACHE"]
  With_universe_dep: _build/default/with_universe_dep.ml
  ["USE_PPX_CACHE"]

This is currently not the case: the cache is enabled for all of them.

The value of an environment variable used in the PPX arguments is part of the
PPX command line, which Merlin includes in its cache key, so the cache can stay
enabled.

  $ cat >> dune <<EOF
  > 
  > (library
  >  (name with_flag_env)
  >  (modules with_flag_env)
  >  (preprocess (pps my_ppx -- --value %{env:SOME_VAR=default})))
  > EOF

  $ echo "let x = 1" > with_flag_env.ml

  $ dune build .merlin-conf/lib-with_flag_env
  $ dune ocaml merlin dump-config --format=json $PWD | jq -r '
  >   include "dune";
  >   merlinEntry("With_flag_env")
  >   | merlinJsonEntryWithConfigNames(["PPX_DEPS", "USE_PPX_CACHE"])
  > '
  With_flag_env: _build/default/with_flag_env
  ["USE_PPX_CACHE"]
  With_flag_env: _build/default/with_flag_env.ml
  ["USE_PPX_CACHE"]
