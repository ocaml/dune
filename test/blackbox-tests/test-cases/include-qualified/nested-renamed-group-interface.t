Nested group interfaces inside renamed directories use their public path.

  $ make_dune_project 3.25

  $ mkdir lib app
  $ cat >lib/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)
  >        (internal/nested as public/exposed)))
  > (library
  >  (name renamed))
  > EOF
  $ mkdir -p lib/internal/nested
  $ cat >lib/internal/leaf.ml <<EOF
  > let value = "from renamed dir"
  > EOF
  $ cat >lib/internal/nested/nested.ml <<EOF
  > let value = "from renamed nested group interface"
  > module Leaf = Leaf
  > EOF
  $ cat >lib/internal/nested/leaf.ml <<EOF
  > let value = "from renamed nested dir"
  > EOF

  $ cat >app/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries renamed))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () =
  >   print_endline Renamed.Public.Leaf.value;
  >   print_endline Renamed.Public.Exposed.value;
  >   print_endline Renamed.Public.Exposed.Leaf.value
  > EOF

  $ dune exec ./app/main.exe
  from renamed dir
  from renamed nested group interface
  from renamed nested dir

Directory renames must preserve path depth.

  $ mkdir bad-depth
  $ cat >bad-depth/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >bad-depth/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public/exposed)))
  > (library
  >  (name bad_depth))
  > EOF
  $ mkdir bad-depth/internal
  $ dune build --root bad-depth
  Entering directory 'bad-depth'
  File "dune", line 3, characters 20-34:
  3 |  (dirs (internal as public/exposed)))
                          ^^^^^^^^^^^^^^
  Error: The source and destination directories must have the same number of
  path components.
  Leaving directory 'bad-depth'
  [1]
