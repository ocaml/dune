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

A rename must not silently overlap an existing directory. This currently
raises an internal error instead of reporting the conflicting groups.

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
  $ dune build --root collision >collision.log 2>&1
  [1]
  $ sed -n 's/.*Assertion.*/Assertion failed/p' collision.log
  Assertion failed

An uppercase source filename should still provide the renamed group
interface. Currently, Internal.ml is treated as a child of Public instead.

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
  Entering directory 'uppercase'
  File "dune", line 6, characters 10-16:
  6 |  (modules Public))
                ^^^^^^
  Error: Module Public doesn't exist.
  Leaving directory 'uppercase'
  [1]

A selected source should likewise provide the renamed group interface.
Currently, its source basename is retained in the logical module path.

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
  Entering directory 'selected'
  File "dune", line 6, characters 10-16:
  6 |  (modules Public)
                ^^^^^^
  Error: Module Public doesn't exist.
  Leaving directory 'selected'
  [1]

The dirs field should only be accepted with qualified mode. Currently it is
silently ignored with both no and unqualified modes.

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

A nested rename may refer to a parent directory visited later. This currently
mistakes the implicit parent group for a conflicting directory.

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
  $ dune build --root=reparented reparented.cma >reparented.log 2>&1
  [1]
  $ sed -n 's/.*Assertion.*/Assertion failed/p' reparented.log
  Assertion failed

A renamed group must not pass through an existing module. Currently the
group is silently omitted.

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

A lexer-generated source should provide the renamed group interface too.
Currently its source basename is retained, as with select-generated sources.

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
  Entering directory 'lexer'
  File "dune", line 6, characters 10-16:
  6 |  (modules Public))
                ^^^^^^
  Error: Module Public doesn't exist.
  Leaving directory 'lexer'
  [1]

Renamed generator targets must not overwrite each other, whether the generators
are declared together or in separate stanzas. Currently both cases succeed.

  $ cp lexer/internal/internal.mll lexer/internal/public.mll
  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal public)
  > EOF
  $ dune build --root=lexer lexer.cma

  $ cat >lexer/internal/dune <<EOF
  > (ocamllex internal)
  > (ocamllex public)
  > EOF
  $ dune build --root=lexer lexer.cma
