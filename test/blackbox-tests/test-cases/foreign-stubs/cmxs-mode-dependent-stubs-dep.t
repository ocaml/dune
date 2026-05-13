Regression test for https://github.com/ocaml/dune/issues/12964

The .cmxa for a library with mode-dependent foreign stubs links against
-l<lib>_stubs_native (via -cclib). The .cmxs build, which links the .cmxa
shared, must therefore depend on libfoo_stubs_native.a — otherwise the link
can race with the native stubs archive build.

  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > (using mode_specific_stubs 0.1)
  > (package (name foo))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name foo)
  >  (foreign_stubs
  >   (language c)
  >   (mode byte)
  >   (names hash))
  >  (foreign_stubs
  >   (language c)
  >   (mode native)
  >   (names stub))
  >  (modules foo))
  > EOF

  $ cat > foo.ml
  $ cat > hash.c
  $ cat > stub.c

The .cmxs rule depends on the native stubs archive, not the byte one.

  $ dune rules --format=json foo.cmxs | \
  >   jq -r 'include "dune"; .[] | ruleDepFilePaths | select(test("stubs"))'
  _build/default/libfoo_stubs_native.a
