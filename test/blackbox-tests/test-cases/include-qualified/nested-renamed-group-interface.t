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

A rename must not overlap an existing directory.

  $ mkdir -p collision/internal collision/public
  $ cat >collision/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >collision/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (name collision))
  > EOF
  $ touch collision/internal/leaf.ml collision/public/other.ml
  $ dune build --root=collision
  Entering directory 'collision'
  File "dune", line 1, characters 0-0:
  Error: Module group "Public" appears in several directories:
  - internal/
  - public/
  Leaving directory 'collision'
  [1]

An uppercase source filename should still provide the renamed group interface.

  $ mkdir -p uppercase/internal
  $ cat >uppercase/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >uppercase/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (name uppercase)
  >  (modules Public))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries uppercase))
  > EOF
  $ cat >uppercase/internal/Internal.ml <<EOF
  > let value = "from uppercase group interface"
  > EOF
  $ cat >uppercase/main.ml <<EOF
  > let () = print_endline Uppercase.Public.value
  > EOF
  $ dune exec --root uppercase ./main.exe
  from uppercase group interface

A selected source should likewise provide the renamed group interface.

  $ mkdir -p selected/internal
  $ cat >selected/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >selected/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (name selected)
  >  (modules Public)
  >  (libraries
  >   (select internal/internal.ml from
  >    (-> internal/internal.fallback.ml))))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries selected))
  > EOF
  $ cat >selected/internal/internal.fallback.ml <<EOF
  > let value = "from selected group interface"
  > EOF
  $ cat >selected/main.ml <<EOF
  > let () = print_endline Selected.Public.value
  > EOF
  $ dune exec --root selected ./main.exe
  from selected group interface

The dirs field should only be accepted with qualified mode.

  $ for mode in no unqualified; do
  >   mkdir -p "$mode/internal"
  >   cat >"$mode/dune-project" <<EOF
  > (lang dune 3.25)
  > EOF
  >   cat >"$mode/dune" <<EOF
  > (include_subdirs
  >  (mode $mode)
  >  (dirs (internal as public)))
  > (library
  >  (name ignored))
  > EOF
  >   touch "$mode/ignored.ml" "$mode/internal/leaf.ml"
  >   dune build --root="$mode" ignored.cma
  > done
  Entering directory 'no'
  File "dune", line 3, characters 1-28:
  3 |  (dirs (internal as public)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The dirs field is only allowed with (mode qualified).
  Leaving directory 'no'
  Entering directory 'unqualified'
  File "dune", line 3, characters 1-28:
  3 |  (dirs (internal as public)))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The dirs field is only allowed with (mode qualified).
  Leaving directory 'unqualified'
  [1]

A nested rename may refer to a parent directory visited later.

  $ mkdir -p reparented/a/nested reparented/public
  $ cat >reparented/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >reparented/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (a/nested as public/nested)))
  > (library (name reparented))
  > EOF
  $ cat >reparented/a/nested/leaf.ml <<EOF
  > let value = 42
  > EOF
  $ cat >reparented/public/public.ml <<EOF
  > let value = Nested.Leaf.value
  > EOF
  $ cat >reparented/reparented.ml <<EOF
  > let value = Public.value
  > EOF
  $ dune build --root=reparented reparented.cma

A renamed group must not pass through an existing module.

  $ mkdir -p prefix/a/nested
  $ cat >prefix/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >prefix/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (a/nested as z/nested)))
  > (library (name prefix))
  > EOF
  $ touch prefix/z.ml prefix/a/nested/leaf.ml
  $ dune build --root=prefix prefix.cma
  Entering directory 'prefix'
  File "dune", line 1, characters 0-0:
  Error: The following module and module group cannot co-exist in the same
  executable or library because they correspond to the same module path
  - module z.ml
  - module group a/nested/
  Leaving directory 'prefix'
  [1]

A lexer-generated source should provide the renamed group interface too.

  $ mkdir -p lexer/internal
  $ cat >lexer/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >lexer/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as public)))
  > (library
  >  (name lexer)
  >  (modules Public))
  > (executable
  >  (name main)
  >  (modules Main)
  >  (libraries lexer))
  > EOF
  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal)
  > EOF
  $ cat >lexer/internal/internal.mll <<EOF
  > rule token = parse
  > | eof { "from generated group interface" }
  > EOF
  $ cat >lexer/main.ml <<EOF
  > let () = print_endline (Lexer.Public.token (Lexing.from_string ""))
  > EOF
  $ dune exec --root=lexer ./main.exe
  from generated group interface

Renamed generator targets must not overwrite each other, whether the generators
are declared together or in separate stanzas.

  $ cp lexer/internal/internal.mll lexer/internal/public.mll
  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal public)
  > EOF
  $ dune build --root=lexer lexer.cma
  Entering directory 'lexer'
  File "internal/dune", line 1, characters 0-26:
  1 | (ocamllex internal public)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Too many files for module Public in internal:
  - _build/default/internal/internal.ml
  - _build/default/internal/public.ml
  Leaving directory 'lexer'
  [1]

  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal)
  > (ocamllex public)
  > EOF
  $ dune build --root=lexer lexer.cma
  Entering directory 'lexer'
  File "internal/dune", line 2, characters 0-17:
  2 | (ocamllex public)
      ^^^^^^^^^^^^^^^^^
  Error: Too many files for module Public in internal:
  - _build/default/internal/internal.ml
  - _build/default/internal/public.ml
  Leaving directory 'lexer'
  [1]

Repeating a generator for the same source still reports conflicting rules.

  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal)
  > (ocamllex internal)
  > EOF
  $ dune build --root=lexer lexer.cma
  Entering directory 'lexer'
  Error: Multiple rules generated for _build/default/internal/internal.ml:
  - internal/dune:2
  - internal/dune:1
  -> required by transitive deps of lexer__Public.impl in _build/default
  -> required by _build/default/.lexer.objs/byte/lexer__Public.cmo
  -> required by _build/default/lexer.cma
  Leaving directory 'lexer'
  [1]
