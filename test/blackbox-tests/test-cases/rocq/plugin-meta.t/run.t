The package layout for plugins is materialized before calling rocqdep. Both
the META file and native plugin are present:
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (rocq.theory
  >  (name bar)
  >  (plugins bar.foo))
  > EOF

  $ dune build .bar.theory.d
  $ find _build/install/default/.packages \( -name META -o -name '*.cmxs' \) \
  >   | sort | censor
  _build/install/default/.packages/$DIGEST/lib/bar/META
  _build/install/default/.packages/$DIGEST/lib/bar/foo/foo.cmxs

Multiple modules and theories share the plugin layout. All modules must still
depend on the plugin's contents, including after an unchanged build.

  $ touch baz.v
  $ mkdir other
  $ cat > other/dune << EOF
  > (rocq.theory
  >  (name qux)
  >  (plugins bar.foo))
  > EOF
  $ touch other/qux.v
  $ compiled_modules() {
  >   dune trace cat | jq -r '
  >     select(.cat == "process" and .name == "finish")
  >     | .args.process_args
  >     | select(.[0] == "compile")
  >     | .[] | select(endswith(".v"))' | sort
  > }

  $ dune build bar.vo baz.vo other/qux.vo
  $ compiled_modules
  bar.v
  baz.v
  other/qux.v

  $ dune build bar.vo baz.vo other/qux.vo
  $ compiled_modules

  $ rm foo.ml
  $ echo 'let foo = "updated"' > foo.ml
  $ dune build bar.vo baz.vo other/qux.vo
  $ compiled_modules
  bar.v
  baz.v
  other/qux.v

  $ dune build bar.vo baz.vo other/qux.vo
  $ compiled_modules
