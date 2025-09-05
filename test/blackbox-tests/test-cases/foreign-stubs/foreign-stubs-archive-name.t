Error when specifying an (archive_name ...) in (foreign_stubs ...) stanza.

  $ echo "(lang dune 2.0)" > dune-project
  $ ./sandboxed.sh

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (archive_name baz) (language c) (names foo))
  >  (foreign_archives bar))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar_stubs.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > EOF

  $ dune build
  File "dune", line 3, characters 16-34:
  3 |  (foreign_stubs (archive_name baz) (language c) (names foo))
                      ^^^^^^^^^^^^^^^^^^
  Error: The field "archive_name" is not allowed in the (foreign_stubs ...)
  stanza. For named foreign archives use the (foreign_library ...) stanza.
  [1]
