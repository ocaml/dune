Testsuite for basic (foreign_library ...) behavior.

* Multiple (foreign_library ...) declarations.
* Mixing C and C++ foreign library archives.
* Passing flags via (flags ...) field.
* Interaction with (foreign_archives ...) stanza.

  $ setup_foreign_library_project

  $ write_calc_dune <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (flags :standard -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ write_basic_calc_project

  $ dune build

  $ dune exec ./main.exe
  2009

  $ (cd _build/default && ocamlrun -I lib main.bc)
  2009

* Include directories via the (include_dirs ...) field.
* Extra dependencies via the (extra_deps ...) field.

  $ write_calc_dune_with_headers_config

  $ write_headers_config_sources

  $ rm -rf _build
  $ dune build

  $ dune exec ./main.exe
  2019

  $ (cd _build/default && ocamlrun -I lib main.bc)
  2019

* Error message when a given (include_dir ...) is not found.

  $ write_calc_dune <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers another/dir)
  >  (extra_deps eight.h)
  >  (flags :standard -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-34:
  12 |  (include_dirs headers another/dir)
                              ^^^^^^^^^^^
  Error: Include directory "another/dir" does not exist.
  [1]

* Error when specifying a non-existing external directory in (include_dirs... )

  $ write_calc_dune <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /some/path)
  >  (extra_deps eight.h)
  >  (flags :standard -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-33:
  12 |  (include_dirs headers /some/path)
                              ^^^^^^^^^^
  Error: Unable to read the include directory.
  Reason: stat(/some/path): No such file or directory
  [1]

* Error when specifying an external file in (include_dirs... )

  $ write_calc_dune <<EOF
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers /bin/sh)
  >  (extra_deps eight.h)
  >  (flags :standard -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 12, characters 23-30:
  12 |  (include_dirs headers /bin/sh)
                              ^^^^^^^
  Error: Unable to read the include directory.
  Reason: "/bin/sh" is not a directory
  [1]

* Error message for multiple declarations with the same "archive_name".

  $ cat >lib/dune <<EOF
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names add))
  > (foreign_library
  >  (archive_name addmul)
  >  (language c)
  >  (names mul))
  > (library
  >  (name calc)
  >  (modules calc)
  >  (foreign_archives addmul config))
  > (foreign_library
  >  (archive_name config)
  >  (language cxx)
  >  (include_dirs headers)
  >  (extra_deps eight.h)
  >  (flags :standard -DCONFIG_VALUE=2000)
  >  (names config))
  > EOF

  $ dune build
  File "lib/dune", line 6, characters 1-22:
  6 |  (archive_name addmul)
       ^^^^^^^^^^^^^^^^^^^^^
  Error: Multiple foreign libraries with the same archive name "addmul"; the
  name has already been taken in lib/dune:2.
  [1]
