Reproduction for https://github.com/ocaml/dune/issues/12964

A library with mode-dependent foreign stubs where the .cmxs build does not
depend on the native stubs archive.

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

Check that the .cmxs rule depends on the native stubs archive.

  $ dune rules --format=json foo.cmxs | \
  >   jq -r 'include "dune"; .[] | ruleDepFilePaths | select(test("stubs"))'
  _build/default/libfoo_stubs_byte.a

This is wrong: the .cmxs links against -lfoo_stubs_native (embedded in the
.cmxa), so it should depend on libfoo_stubs_native.a, not libfoo_stubs_byte.a.
This causes a race condition where the .cmxs link can run before the native
stubs archive is built.
