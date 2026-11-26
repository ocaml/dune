----------------------------------------------------------------------------------
Test where a single include directory is added via a `(include ...)` statement

----------------------------------------------------------------------------------
* Versions of dune before 3.5 do not support this feature

  $ make_dune_project 3.4
  $ make_foreign_stubs_include_dirs_project
  $ dune build
  File "dune", line 6, characters 16-29:
  6 |   (include_dirs (include foo))))
                      ^^^^^^^^^^^^^
  Error: 'include' is only available since version 3.5 of the dune language.
  Please update your dune-project file to have (lang dune 3.5).
  [1]

----------------------------------------------------------------------------------
* Error if include file is missing

  $ make_dune_project 3.5

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > value bar(value unit) { return Val_int(A); }
  > EOF

  $ dune build
  File "dune", line 5, characters 9-12:
  5 |   (names bar)
               ^^^
  Error: No rule found for foo
  [1]

----------------------------------------------------------------------------------
* Simple example

  $ mkdir -p inc_a
  $ echo "#define A 42" > inc_a/a.h
  $ echo inc_a > foo

  $ dune build

An absolute include directory must be tracked recursively too. Changing only
an external nested header should rebuild the stub and relink the executable.

  $ make_dune_project 3.26
  $ cat > dune <<'EOF'
  > (executable
  >  (name foo)
  >  (foreign_stubs (language c) (names bar) (include_dirs (include foo))))
  > EOF
  $ include_dir=$(mktemp -d)
  $ mkdir "$include_dir/nested"
  $ echo '#define A 42' > "$include_dir/nested/a.h"
  $ printf '%s\n' "$include_dir" > foo
  $ cat > bar.c <<'EOF'
  > #include <caml/mlvalues.h>
  > #include <nested/a.h>
  > value bar(value unit) { return Val_int(A); }
  > EOF
  $ cat > foo.ml <<'EOF'
  > external bar : unit -> int = "bar"
  > let () = Printf.printf "%d\n" (bar ())
  > EOF
  $ dune exec --cache=disabled --sandbox=hardlink ./foo.exe
  42
  $ echo '#define A 71' > "$include_dir/nested/a.h"

CR: The incremental build still runs the old code. A fresh build updates it.

  $ dune exec --cache=disabled --sandbox=hardlink ./foo.exe
  42
  $ dune exec --cache=disabled --sandbox=hardlink --build-dir _fresh ./foo.exe
  71

