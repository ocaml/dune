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

The mounted and workspace consumers all select the same provider.

  $ dune build main_left.exe main_right.exe workspace_main.exe
  $ _build/default/main_left.exe
  left/workspace
  $ _build/default/main_right.exe
  right/workspace
  $ _build/default/workspace_main.exe
  workspace

They can also link together without introducing duplicate compilation units.

  $ cat >> dune <<'EOF'
  > (executable (name main) (modules main) (libraries left right support))
  > EOF
  $ dune build main.exe
  $ _build/default/main.exe
  left/workspace/right/workspace
  $ test ! -e _build/default/.lockfile/pkg/left/other/support.cmxa
  $ test ! -e _build/default/.lockfile/pkg/right/other/support.cmxa

Installation preserves the declared dependency, without adopting its library.

  $ dune build @pkg-install
  $ grep '^requires' _build/default/.lockfile/pkg/left/META.left
  requires = "support"
  $ grep '^requires' _build/default/.lockfile/pkg/right/sub/META.right
  requires = "support"
  $ find _build/_private/default/.packages -path '*/__private__/support/support.cmxa' | wc -l
  0

Managed dependencies must have the same visibility regardless of their builder.
Neither lock package below declares a dependency on the other.

  $ echo '(executable (name main_left) (modules main_left) (libraries left))' > dune
  $ echo '(library (public_name left) (libraries right))' > _shared/dune
  $ echo 'let message = Right.message' > _shared/left.ml
  $ dune build main_left.exe
  File "_build/default/.lockfile/pkg/left/dune", line 1, characters 39-44:
  Error: Library "right" not found.
  -> required by library "left" in _build/default/.lockfile/pkg/left
  -> required by executable main_left in dune:1
  -> required by
     _build/default/.main_left.eobjs/native/dune__exe__Main_left.cmx
  -> required by _build/default/main_left.exe
  [1]

The same undeclared provider is hidden when it is opaque. Its recipe
must not be run while resolving left's libraries.

  $ make_lockpkg right <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (progn (run false) (patch missing.patch)))
  > EOF
  $ dune build main_left.exe
  File "_build/default/.lockfile/pkg/left/dune", line 1, characters 39-44:
  Error: Library "right" not found.
  -> required by library "left" in _build/default/.lockfile/pkg/left
  -> required by executable main_left in dune:1
  -> required by
     _build/default/.main_left.eobjs/native/dune__exe__Main_left.cmx
  -> required by _build/default/main_left.exe
  [1]

A declared transitive dependency makes the native provider visible without
building the metadata-only intermediate package.

  $ make_lockpkg right <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  $ make_lockpkg bridge <<'EOF'
  > (version 1.0)
  > (depends right)
  > (build (run false))
  > EOF
  $ make_lockpkg left <<EOF
  > (version 1.0)
  > (depends bridge)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  $ dune build main_left.exe
  $ _build/default/main_left.exe
  right/workspace
  $ test ! -e _build/default/.lockfile/pkg/bridge/.opam/bridge/target

Native eligibility belongs to the selected package's project, not its snapshot.
A package with no Dune rules initially uses its recipe.

  $ mkdir _eligibility
  $ printf '(lang dune 3.25)\n(package (name probe))\n' > _eligibility/dune-project
  $ make_lockpkg probe <<EOF
  > (version 1.0)
  > (source (copy $PWD/_eligibility))
  > (build (system "mkdir -p %{prefix}/share/probe; echo opaque > %{prefix}/share/probe/marker"))
  > EOF
  $ dune build _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  $ cat _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  opaque

Adding an unrelated nested project must not change that choice.

  $ mkdir _eligibility/unrelated
  $ printf '(lang dune 3.25)\n(package (name unrelated))\n' > _eligibility/unrelated/dune-project
  $ echo '(rule (target output) (action (write-file %{target} unrelated)))' > _eligibility/unrelated/dune
  $ dune build _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  $ cat _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  opaque

Rules in a subdirectory of the selected project do make it eligible.

  $ mkdir _eligibility/own
  $ echo '(rule (target output) (action (write-file %{target} native)))' > _eligibility/own/dune
  $ dune build _build/default/.lockfile/pkg/probe/own/output
  $ cat _build/default/.lockfile/pkg/probe/own/output
  native
