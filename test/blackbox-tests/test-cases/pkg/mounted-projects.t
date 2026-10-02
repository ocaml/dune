Selected packages sharing a source snapshot have separate output roots. Nested
projects retain their relative paths, and each view hides the other selected
package. Cram test fixtures are data-only before project parsing.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends left right))
  > EOF
  $ echo '(executable (name main) (libraries left right))' > dune
  $ echo 'let () = Printf.printf "%s/%s\n" Left.message Right.message' > main.ml
  $ mkdir -p _shared/sub _shared/invalid.t
  $ cat > _shared/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name left))
  > EOF
  $ cat > _shared/dune <<'EOF'
  > (library (public_name left))
  > (cram (applies_to :whole_subtree))
  > EOF
  $ echo 'let message = "left"' > _shared/left.ml
  $ echo '(' > _shared/invalid.t/dune-project
  $ cat > _shared/sub/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name right))
  > EOF
  $ echo '(library (public_name right))' > _shared/sub/dune
  $ echo 'let message = "right"' > _shared/sub/right.ml
  $ for package in left right; do
  >   make_lockpkg "$package" <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  > done

  $ dune build main.exe
  $ _build/default/main.exe
  left/right
  $ find _build/_fetch -type d -name dir | wc -l
  1
  $ test -f _build/default/.lockfile/pkg/left/left.cmxa
  $ test -f _build/default/.lockfile/pkg/right/sub/right.cmxa
  $ test ! -e _build/default/.lockfile/pkg/left/sub/right.cmxa
  $ test ! -e _build/default/.lockfile/pkg/right/left.cmxa

Other packages in the same snapshot are not selected merely by being present.
In particular, their libraries must not replace a selected workspace provider
or be reassigned to the mounted package during installation.

  $ mkdir workspace-support
  $ cat > workspace-support/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name support))
  > EOF
  $ echo '(library (public_name support))' > workspace-support/dune
  $ echo 'let message = "workspace"' > workspace-support/support.ml
  $ mkdir -p _shared/other
  $ cat > _shared/other/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name support))
  > (package (name unused))
  > EOF
  $ cat > _shared/other/dune <<'EOF'
  > (library (public_name support) (modules support))
  > (library (public_name unused) (modules unused) (libraries missing))
  > EOF
  $ echo 'let message = "unselected"' > _shared/other/support.ml
  $ echo 'let value = ()' > _shared/other/unused.ml
  $ echo '(library (public_name left) (libraries support))' > _shared/dune
  $ echo '(library (public_name right) (libraries support))' > _shared/sub/dune
  $ echo 'let message = "left/" ^ Support.message' > _shared/left.ml
  $ echo 'let message = "right/" ^ Support.message' > _shared/sub/right.ml
  $ cat > dune <<'EOF'
  > (executable (name main_left) (modules main_left) (libraries left))
  > (executable (name main_right) (modules main_right) (libraries right))
  > (executable (name workspace_main) (modules workspace_main) (libraries support))
  > EOF
  $ echo 'let () = print_endline Left.message' > main_left.ml
  $ echo 'let () = print_endline Right.message' > main_right.ml
  $ echo 'let () = print_endline Support.message' > workspace_main.ml

BUG: the unselected source package collides with the workspace provider during
scope construction. Keep only the diagnostic, not the internal stack trace.

  $ dune build main_left.exe main_right.exe workspace_main.exe 2>error
  [1]
  $ grep '("Map.of_list_exn"' error
    ("Map.of_list_exn", { key = "support" })

Installation encounters the same collision.

  $ dune build @pkg-install 2>error
  [1]
  $ grep '("Map.of_list_exn"' error
    ("Map.of_list_exn", { key = "support" })
