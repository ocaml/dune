Explicit language lists require Dune 3.26.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c cxx))
  > EOF
  $ dune build
  File "dune", line 3, characters 1-18:
  3 |  (languages c cxx))
       ^^^^^^^^^^^^^^^^^
  Error: 'languages' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]

Language lists are nonempty and mutually exclusive with language.

  $ make_dune_project 3.26
  $ echo '(foreign_library (archive_name x) (languages))' > dune
  $ dune build
  File "dune", line 1, characters 34-45:
  1 | (foreign_library (archive_name x) (languages))
                                        ^^^^^^^^^^^
  Error: Not enough arguments for "languages"
  [1]
  $ echo '(foreign_library (archive_name x) (language c) (languages c))' > dune
  $ dune build
  File "dune", line 1, characters 0-61:
  1 | (foreign_library (archive_name x) (language c) (languages c))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: fields "language" and "languages" are mutually exclusive.
  [1]

Selected C and C++ sources cannot share an object name.

  $ touch foo.c foo.cpp
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c cxx)
  >  (names foo))
  > EOF
  $ dune build
  File "dune", line 4, characters 8-11:
  4 |  (names foo))
              ^^^
  Error: Multiple sources map to the same object name "foo":
  - foo.c
  - foo.cpp
  This is not allowed; please rename them or remove "foo" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

Selecting C does not accept a C++ source with the same object name.

  $ rm foo.c
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages c)
  >  (names foo))
  > EOF
  $ dune build
  File "dune", line 4, characters 8-11:
  4 |  (names foo))
              ^^^
  Error: Object "foo" has no source; "foo.c" must be present.
  [1]

Selecting C++ does not accept a C source with the same object name.

  $ mv foo.cpp foo.c
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name mixed)
  >  (languages cxx)
  >  (names foo))
  > EOF
  $ dune build
  File "dune", line 4, characters 8-11:
  4 |  (names foo))
              ^^^
  Error: Object "foo" has no source; One of "foo.cc", "foo.cpp" or "foo.cxx"
  must be present.
  [1]
